package io.kaitai.struct.testtranslator.specgenerators

import _root_.io.kaitai.struct.{ClassTypeProvider, JSON, RuntimeConfig}
import _root_.io.kaitai.struct.datatype.{DataType, KSError}
import _root_.io.kaitai.struct.exprlang.Ast
import _root_.io.kaitai.struct.languages.RustCompiler
import _root_.io.kaitai.struct.translators.RustTranslator
import _root_.io.kaitai.struct.datatype.DataType.{BytesType, EnumType, Int1Type, IntType, SwitchType, UserType}
import _root_.io.kaitai.struct.format.ClassSpecs
import io.kaitai.struct.testtranslator.Main.CLIOptions
import io.kaitai.struct.testtranslator.{Main, TestAssert, TestSpec}

class RustSG(spec: TestSpec, provider: ClassTypeProvider, classSpecs: ClassSpecs, options: CLIOptions) extends BaseGenerator(spec) {
  val className: String = RustCompiler.type2class(spec.id)
  val translator = new RustTranslator(provider, RuntimeConfig())
  var do_panic = true

  override def fileName(name: String): String = s"test_$name.rs"

  override def header(): Unit = {
    val use_mod = if (options.unitTest)
                    s"use crate::"
                  else
                    s"mod formats;\nuse "
    var imports = ""
    spec.extraImports.foreach{ name => imports = s"$imports\n${use_mod}formats::$name::*;"  }

    val code =
      s"""|use std::fs;
          |
          |extern crate kaitai;
          |use self::kaitai::*;
          |${use_mod}formats::${spec.id}::*;
          |$imports
          |
          |#[test]
          |fn test_${spec.id}() {
          |    let bytes = fs::read("../../src/${spec.data}").unwrap();
          |    let reader = BytesReader::new(&bytes);
          |    let mut r = $className::default();
          |
          |    if let Err(err) = r.read(&reader, None, Some(KStructUnit::parent_stack())) {""".stripMargin
    out.puts(code)
    out.inc
  }

  override def runParse(): Unit = {
    finish_panic()
  }

  override def runParseExpectError(exception: KSError): Unit = {
    val code =
      s"""    println!("expected err: {:?}, exception: $exception", err);
      |    } else {
      |        panic!("no expected exception: $exception");
      |    }""".stripMargin
    out.puts(code)
    do_panic = false
  }

  def finish_panic(): Unit = {
    if (do_panic) {
      out.inc
      out.puts("panic!(\"{:?}\", err);")
      out.dec
      out.puts("}")
      do_panic = false
    }
  }
  override def footer(): Unit = {
    out.dec
    out.puts("}")
  }

  override def simpleAssert(check: TestAssert): Unit = {
    import io.kaitai.struct.datatype.DataType.{BooleanType => DTBooleanType, IntType => DTIntType, NumericType => DTNumericType}

    val actType = translator.detectType(check.actual)
    var actStr = translateAct(check.actual)
    val expType = translator.detectType(check.expected)
    var expStr = translate(check.expected)
/*
    (actType, expType) match {
      case (_: EnumType, _: EnumType) =>
        //if (actStr.endsWith(".to_owned()")) {
          expStr = s"&${remove_ref(expStr)}"
          actStr = translator.remove_deref(actStr)
//        } else {
//          actStr = translator.remove_ref(actStr)
//        }
//      case (_: EnumType, _: DTBooleanType) =>
//        expStr = remove_ref(expStr)
      case (_: EnumType, _: DTIntType) =>
        actStr = s"${translator.remove_deref(actStr)}.clone().to_owned() as u64"
      case (_: EnumType, _: Int1Type) =>
        actStr = s"${translator.remove_deref(actStr)}.clone().to_owned() as u8"
      case (_, _: DTNumericType) | (_, _: DTBooleanType)=>
        if(!(actStr.endsWith(".len()") || actStr.endsWith(".to_owned()") || actStr.endsWith("]"))) {
          expStr = translator.ensure_ref(expStr)
        }
        actStr = translator.remove_deref(actStr)
//        if(actStr.startsWith("&")) {
//          expStr = s"&$expStr"
//        }
      case _ =>
    }
*/
    // fix expStr as vector
    if(expStr.startsWith("&vec![")) {
      if (actStr.charAt(0) == '*') {
        expStr = remove_ref(expStr)
      }
    }

    finish_panic()
    out.puts(s"assert_eq!($actStr, $expStr);")
  }

  override def nullAssert(actual: Ast.expr): Unit = {
    val actStr = translateAct(actual)
    val expStr = if(actStr.startsWith("&")) "&0" else "0"
    finish_panic()
    out.puts(s"assert_eq!($actStr, $expStr);")
    // TODO: Figure out what's meant to happen here
  }

  override def trueArrayAssert(check: TestAssert, elType: DataType, elts: Seq[Ast.expr]): Unit = {
    simpleAssert(check) // FIXME
  }

  override def indentStr: String = "    "

  override def results: String = {
    "// " + AUTOGEN_COMMENT + "\n\n" +
      out.result
  }

  def translate(x: Ast.expr): String = {
    //TODO: correct code generation
    def correctReader(code: String): String =
      code.replace("_io)?", "&reader).unwrap()")

    var ttx = translator.translate(x)
    // append (&reader).unwrap() to instance call
    val dots = ttx.split("\\.")
    var ttx2 = dots(0)
    var last = ""
    var last_full = ""
    dots.drop(1).foreach {
      attr_full =>
        last_full = attr_full
        val ind = attr_full indexOf "()"
        if (ind > 0) {
          val attr = attr_full.substring(0, ind)
          last = attr
          val found = translator.get_instance(translator.get_top_class(classSpecs.firstSpec), attr)
          if (found.isDefined) {
            ttx2 = s"$ttx2.$attr(&reader).unwrap()${attr_full.substring(ind + 2, attr_full.length())}"
          } else {
            ttx2 = s"$ttx2.$attr_full"
          }
        } else {
          ttx2 = s"$ttx2.$attr_full"
        }
    }
    // do we need to deref?
    if (last.nonEmpty) {
      var deref = true
      if (last == "len" || last_full.contains("[")) {
        deref = false
      } else {
        val found = translator.get_attr(translator.get_top_class(classSpecs.firstSpec), last)
        if (found.isDefined) {
          deref = found.get.dataTypeComposite match {
            case _: SwitchType => false
            case _: UserType => false
            case _: BytesType => false
            //case _: IntType  => false
            case _ => true
          }
        } else if (translator.get_instance(translator.get_top_class(classSpecs.firstSpec), last).isDefined)  {
          deref = true
        } else if (translator.get_param(translator.get_top_class(classSpecs.firstSpec), last).isDefined)  {
          deref = true
        } else {
          deref = !(last == "to_string" /*|| last == "to_owned"*/ || last == "as_ref")
        }
      }
      ttx = if (deref) {
        translator.ensure_deref(ttx2)
      } else {
        s"${translator.remove_deref(ttx2)}"
      }
    }

    correctReader(ttx)
  }

  def remove_ref(s: String): String = {
    if (s.charAt(0) == '&') {
      s.substring(1)
    } else {
      s
    }
  }

  def translateAct(x: Ast.expr): String =
    translate(x).replaceAll(s"self.${Main.INIT_OBJ_NAME}(\\(\\))?", "r")
}
