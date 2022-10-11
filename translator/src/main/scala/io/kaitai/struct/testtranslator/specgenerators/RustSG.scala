package io.kaitai.struct.testtranslator.specgenerators

import _root_.io.kaitai.struct.{ClassTypeProvider, JSON, RuntimeConfig}
import _root_.io.kaitai.struct.datatype.{DataType, KSError}
import _root_.io.kaitai.struct.exprlang.Ast
import _root_.io.kaitai.struct.languages.RustCompiler
import _root_.io.kaitai.struct.translators.RustTranslator
import _root_.io.kaitai.struct.datatype.DataType.{BytesType, EnumType, Int1Type, IntType, SwitchType, UserType}
import _root_.io.kaitai.struct.format.{ClassSpecs, NamedIdentifier}
import io.kaitai.struct.testtranslator.Main.CLIOptions
import io.kaitai.struct.testtranslator.{Main, TestAssert, TestSpec}

import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

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
    def correctReader(code: String): String = {
      val res = code.replace("_io)?", "&reader).unwrap()")
      res
    }

    def isInstance(item: String): (Boolean, String) = {
      val ATTR_NAME_RE = "([^()]+)\\((_io)*\\)?".r
      val it = ATTR_NAME_RE.findAllMatchIn(item)
      if (it.hasNext) {
        val attr = it.next.group(1)
        val res = translator.get_instance(translator.get_top_class(classSpecs.firstSpec), attr).isDefined
        (res, attr)
      } else {
        (false, "")
      }
    }

    def getAttrType(attr: String): Option[DataType] = {
      val found = translator.get_attr(translator.get_top_class(classSpecs.firstSpec), attr)
      if (found.isDefined) {
         Some(found.get.dataTypeComposite)
      } else {
        None
      }
    }

    var ttx = translator.translate(x)
    // append (&reader).unwrap() to instance call
    var dots = ArrayBuffer[String]() ++= ttx.split("\\.")
    val lastNo = dots.length - 1
    for (i <- 1 to lastNo) {
      var deref = true
      val (inst, attr) = isInstance(dots(i))

      dots(i) = dots(i).replace("_io)?", "&reader).unwrap().to_owned())")
      var removeMut = !(inst || ((i > 2) && dots(i).startsWith("to_owned")))

      if (i == lastNo) {
        if (attr == "len" || dots(i).contains("[")) {
          deref = false
        } else {
//          val attrMember = translator.findMember(attr)
//          if (attrMember.isDefined) {
//            val valType = translator.provider.determineType(attrMember.get._1, attr)
//            val (res, _, _) = RustCompiler.attributeNativeType(NamedIdentifier(attr), valType, provider.asInstanceOf[ClassTypeProvider], false)
//            deref = res.startsWith("Ref<")
//          }
          val opt_type = getAttrType(attr)
          deref = opt_type.isDefined && translator.is_copy_type(opt_type.get)
        }

        if (removeMut) {
          val j = dots.indexOf("to_owned())")
          if (j > 0) {
            dots(j) = "to_owned()"
          }
        } else {
          dots(lastNo - 1) = dots(lastNo - 1).replace("))", ")")
          dots.remove(lastNo)
          dots.remove(dots.indexOf("to_owned())"))
          val j = dots.lastIndexWhere{ _.endsWith("to_owned())") }
          dots(j) = dots(j).replace(".to_owned())", "")
        }

        var first = if (removeMut) {
          dots(0).replace("(&mut ", "")
        } else {
          dots(0).replace("((", "*(")
        }

        dots(0) = if (deref) {
          translator.ensure_deref(first, forSelfOnly = false)
        } else {
          s"${translator.remove_deref(first)}"
        }
      }
    }

    return dots.mkString(".")
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
        if (last != "to_owned") {
          dots(0) = "*self"
          val penult = dots.length - 2
          dots(penult) = dots(penult).substring(0, dots(penult).length - 1)
          return correctReader(dots.mkString("."))
        } else {
          dots(0) = s"*${dots(0).substring(1)}"
          val (l,r) = dots.splitAt(2)
          dots = l ++ r.drop(1)
          val (l1,r1) = dots.splitAt(3)
          dots = l1 ++ r.take(1) ++ r1
          dots = dots.take(dots.length - 1)
          val last = dots.length - 1
          dots(last) = dots(last).substring(0, dots(last).length - 1)
          return correctReader(dots.mkString("."))
        }
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

  def translateAct(x: Ast.expr): String = {
    val code = translate(x)
    code.replaceAll(s"self.${Main.INIT_OBJ_NAME}(\\(\\))?", "r")
  }
}
