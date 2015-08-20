package io.getquill.norm.select

import scala.reflect.macros.whitebox.Context

import io.getquill.ast.Ast
import io.getquill.ast.AstShow.astShow
import io.getquill.ast.Property
import io.getquill.ast.Query
import io.getquill.ast.Tuple
import io.getquill.ast.AstShow
import io.getquill.util.Messages.RichContext
import io.getquill.util.Show.Shower

trait SelectFlattening extends SelectValues {
  val c: Context

  import c.universe._

  protected def flattenSelect[T](q: Query, inferDecoder: Type => Option[Tree])(implicit t: WeakTypeTag[T]) = {
    val (query, mapAst) = ExtractSelect(q)
    val selectValues =
      selectElements(mapAst).map {
        case (ast, typ) =>
          inferDecoder(typ) match {
            case Some(decoder) =>
              SimpleSelectValue(ast, decoder)
            case None if (typ.typeSymbol.asClass.isCaseClass) =>
              caseClassSelectValue(typ, ast, inferDecoder)
            case _ =>
              import AstShow._
              c.fail(s"Source doesn't know how to decode '${t.tpe.typeSymbol.name}.${ast.show}: $typ'")
          }
      }
    (ReplaceSelect(query, selectAsts(selectValues).flatten), selectValues)
  }

  private def selectAsts(values: List[SelectValue]) =
    values map {
      case SimpleSelectValue(ast, _)      => List(ast)
      case CaseClassSelectValue(_, params) => params.flatten.map(_.ast)
    }

  private def caseClassSelectValue(typ: Type, ast: Ast, inferDecoder: Type => Option[Tree]) =
    CaseClassSelectValue(typ, selectValuesForCaseClass(typ, ast, inferDecoder))

  private def selectValuesForCaseClass(typ: Type, ast: Ast, inferDecoder: Type => Option[Tree]) =
    selectValuesForConstructor(caseClassConstructor(typ), ast, inferDecoder)

  private def selectValuesForConstructor(constructor: MethodSymbol, ast: Ast, inferDecoder: Type => Option[Tree]) =
    constructor.paramLists.map(_.map {
      param =>
        val paramType = param.typeSignature.typeSymbol.asType.toType
        val decoder =
          inferDecoder(paramType)
            .getOrElse(c.fail(s"Source doesn't know how to decode '${param.name}: $paramType'"))
        SimpleSelectValue(Property(ast, param.name.decodedName.toString), decoder)
    })

  private def selectElements[T](mapAst: Ast)(implicit t: WeakTypeTag[T]) =
    mapAst match {
      case Tuple(values) =>
        if (values.size != t.tpe.typeArgs.size)
          c.fail(s"Query shape doesn't match the return type $t, please submit a bug report.")
        values.zip(t.tpe.typeArgs)
      case ast =>
        List(ast -> t.tpe)
    }

  private def caseClassConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if (m.isPrimaryConstructor) => m
    }.headOption.getOrElse {
      c.fail(s"Can't find the primary constructor for '${t.typeSymbol.name}, please submit a bug report.'")
    }

}