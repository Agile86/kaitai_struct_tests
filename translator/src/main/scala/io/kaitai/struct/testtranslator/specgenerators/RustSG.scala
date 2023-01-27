package io.kaitai.struct.testtranslator.specgenerators

import _root_.io.kaitai.struct.{ClassTypeProvider, JSON, RuntimeConfig}
import _root_.io.kaitai.struct.datatype.{DataType, KSError}
import _root_.io.kaitai.struct.exprlang.Ast
import _root_.io.kaitai.struct.languages.RustCompiler
import _root_.io.kaitai.struct.translators.RustTranslator
import _root_.io.kaitai.struct.datatype.DataType.{ArrayType, BooleanType, BytesType, EnumType, IntType, SwitchType, UserType}
import _root_.io.kaitai.struct.format.ClassSpecs
import io.kaitai.struct.testtranslator.Main.CLIOptions
import io.kaitai.struct.testtranslator.{Main, TestAssert, TestSpec}

class RustSG(spec: TestSpec, provider: ClassTypeProvider, classSpecs: ClassSpecs, options: CLIOptions) extends BaseGenerator(spec) {
  val className: String = RustCompiler.type2class(spec.id)
  val translator = new RustTranslator(provider, RuntimeConfig())
  var do_panic = true
  var do_not_deref = false

  override def fileName(name: String): String = s"test_$name.rs"

  override def header(): Unit = {
    val use_mod = if (!options.unitTest) "mod formats;" else ""
    var imports = ""
    spec.extraImports.foreach{ name => imports = s"$imports\n    use crate::formats::$name::*;" }

    val code =
      s"""|#![allow(unused_variables)]
          |#![allow(unused_assignments)]
          |#![allow(unused_imports)]
          |
          |$use_mod
          |
          |#[cfg(test)]
          |mod tests {
          |    use std::{fs, rc::Rc};
          |
          |    extern crate kaitai;
          |    use self::kaitai::*;
          |    use crate::formats::${spec.id}::*;$imports
          |
          |    #[test]
          |    fn test_${spec.id}() -> Result<(), KError> {
          |        let bytes = fs::read("../../src/${spec.data}").unwrap();
          |        let _io = &BytesReader::new(&bytes);
          |        let res: KResult<Rc<$className>> = $className::read_into(_io, None, None);
          |        let r: Rc<$className>;
          |
          |        if let Err(err) = res {""".stripMargin
    out.puts(code)
    out.inc
  }

  override def runParse(): Unit = {
    out.inc
    finish_panic()
  }

  override def runParseExpectError(exception: KSError): Unit = {
    out.inc
    val code =
      s"""    println!("expected err: {:?}, exception: $exception", err);
          |        } else {
          |            panic!("no expected exception: $exception");
          |        }
          |""".stripMargin
    out.puts(code)
    do_panic = false
  }

  def finish_panic(): Unit = {
    if (do_panic) {
      out.inc
      out.puts("panic!(\"{:?}\", err);")
      out.dec
      out.puts("} else {")
      out.inc
      out.puts("r = res.unwrap();")
//      out.dec
//      out.puts("}")
      do_panic = false
    }
  }
  override def footer(): Unit = {
    while (out.indentLevel > 0) {
      if(out.indentLevel == 2)
        out.puts("Ok(())")
      out.dec
      out.puts("}")
    }
  }

  def correctIO(code: String): String = {
    if (!do_not_deref) {
      if (code.contains("_io,") && (code.charAt(0) != '*'))  s"*$code" else code
    } else {
      code
    }
  }

  override def simpleAssert(check: TestAssert): Unit = {
    val actType = translator.detectType(check.actual)
    var actStr = translateAct(check.actual)
    val expType = translator.detectType(check.expected)
    var expStr = translate(check.expected)
    (actType, expType) match {
      case (at: EnumType, et: EnumType) =>
        expStr = remove_ref(expStr)
      case (at: EnumType, et: BooleanType) =>
        expStr = remove_ref(expStr)
      case (at: EnumType, et: IntType) =>
        out.puts(s"let n : i64 = (&$actStr).into();")
        actStr = s"n"
      case _ =>
    }
    // fix expStr as vector
    if (actStr.charAt(0) == '*' && expStr.startsWith("&vec![")) {
      expStr = remove_ref(expStr)
    }
    finish_panic()
    //TODO: correct code generation
    actStr = correctIO(actStr)

    out.puts(s"assert_eq!($actStr, $expStr);")
  }

  override def nullAssert(actual: Ast.expr): Unit = {
    val actStr = correctIO(translateAct(actual))
    finish_panic()
    out.puts(s"assert_eq!($actStr, 0);")
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
    val ttx = translator.translate(x)
    val dots = ttx.split("\\.")
    var ttx2 = dots(0)
    var last = ""
    var last_full = ""
    dots.drop(1).foreach {
      attr_full =>
        last_full = attr_full
        val ind = attr_full indexOf "("
        if (ind > 0) {
          val attr = attr_full.substring(0, ind)
          last = attr
        }
        ttx2 = s"$ttx2.$attr_full"
    }
    // do we need to deref?
    if (last.nonEmpty) {
      var deref = true
      if (last == "len" || last_full.contains("[")) {
        deref = dots.take(dots.size - 1).last.contains("[")
        do_not_deref = true
      } else {
        if (last != "bytes")
          deref = translator.need_deref(last)
      }
      if (deref) {
        if (ttx2.charAt(0) == '*') {
          ttx2
        } else {
          s"*$ttx2"
        }
      } else {
        s"${translator.remove_deref(ttx2)}"
      }
    } else {
      ttx
    }
  }

  def remove_ref(s: String): String = {
    if (s.charAt(0) == '&') {
      s.substring(1)
    } else {
      s
    }
  }

  def translateAct(x: Ast.expr): String =
    translate(x).replace(s"self.${Main.INIT_OBJ_NAME}()", "r")
}
