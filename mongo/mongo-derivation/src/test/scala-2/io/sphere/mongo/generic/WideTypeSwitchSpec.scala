package io.sphere.mongo.generic

import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format.MongoFormat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** A sealed trait far wider than `mongoTypeSwitch` used to support: the generated overloads capped
  * it at ~125 subtypes via the JVM method-parameter limit. If that ceiling comes back, this stops
  * compiling.
  *
  * Each subtype's instance lives in its own companion, so the derivations land in 200 tiny class
  * initialisers instead of one huge one.
  *
  * scala-2 only until the scala 3 side takes a selector list too.
  */
class WideTypeSwitchSpec extends AnyWordSpec with Matchers {
  import WideTypeSwitchSpec._

  "mongoTypeSwitch" must {
    s"round-trip all ${values.size} subtypes" in
      values.foreach(v => mongo.fromMongoValue(mongo.toMongoValue(v)) must be(v))
  }
}

object WideTypeSwitchSpec {
  sealed trait Wide
  // format: off
  case class C1(x: Int) extends Wide
  object C1 { implicit val mongo: MongoFormat[C1] = deriveMongoFormat }
  case class C2(x: Int) extends Wide
  object C2 { implicit val mongo: MongoFormat[C2] = deriveMongoFormat }
  case class C3(x: Int) extends Wide
  object C3 { implicit val mongo: MongoFormat[C3] = deriveMongoFormat }
  case class C4(x: Int) extends Wide
  object C4 { implicit val mongo: MongoFormat[C4] = deriveMongoFormat }
  case class C5(x: Int) extends Wide
  object C5 { implicit val mongo: MongoFormat[C5] = deriveMongoFormat }
  case class C6(x: Int) extends Wide
  object C6 { implicit val mongo: MongoFormat[C6] = deriveMongoFormat }
  case class C7(x: Int) extends Wide
  object C7 { implicit val mongo: MongoFormat[C7] = deriveMongoFormat }
  case class C8(x: Int) extends Wide
  object C8 { implicit val mongo: MongoFormat[C8] = deriveMongoFormat }
  case class C9(x: Int) extends Wide
  object C9 { implicit val mongo: MongoFormat[C9] = deriveMongoFormat }
  case class C10(x: Int) extends Wide
  object C10 { implicit val mongo: MongoFormat[C10] = deriveMongoFormat }
  case class C11(x: Int) extends Wide
  object C11 { implicit val mongo: MongoFormat[C11] = deriveMongoFormat }
  case class C12(x: Int) extends Wide
  object C12 { implicit val mongo: MongoFormat[C12] = deriveMongoFormat }
  case class C13(x: Int) extends Wide
  object C13 { implicit val mongo: MongoFormat[C13] = deriveMongoFormat }
  case class C14(x: Int) extends Wide
  object C14 { implicit val mongo: MongoFormat[C14] = deriveMongoFormat }
  case class C15(x: Int) extends Wide
  object C15 { implicit val mongo: MongoFormat[C15] = deriveMongoFormat }
  case class C16(x: Int) extends Wide
  object C16 { implicit val mongo: MongoFormat[C16] = deriveMongoFormat }
  case class C17(x: Int) extends Wide
  object C17 { implicit val mongo: MongoFormat[C17] = deriveMongoFormat }
  case class C18(x: Int) extends Wide
  object C18 { implicit val mongo: MongoFormat[C18] = deriveMongoFormat }
  case class C19(x: Int) extends Wide
  object C19 { implicit val mongo: MongoFormat[C19] = deriveMongoFormat }
  case class C20(x: Int) extends Wide
  object C20 { implicit val mongo: MongoFormat[C20] = deriveMongoFormat }
  case class C21(x: Int) extends Wide
  object C21 { implicit val mongo: MongoFormat[C21] = deriveMongoFormat }
  case class C22(x: Int) extends Wide
  object C22 { implicit val mongo: MongoFormat[C22] = deriveMongoFormat }
  case class C23(x: Int) extends Wide
  object C23 { implicit val mongo: MongoFormat[C23] = deriveMongoFormat }
  case class C24(x: Int) extends Wide
  object C24 { implicit val mongo: MongoFormat[C24] = deriveMongoFormat }
  case class C25(x: Int) extends Wide
  object C25 { implicit val mongo: MongoFormat[C25] = deriveMongoFormat }
  case class C26(x: Int) extends Wide
  object C26 { implicit val mongo: MongoFormat[C26] = deriveMongoFormat }
  case class C27(x: Int) extends Wide
  object C27 { implicit val mongo: MongoFormat[C27] = deriveMongoFormat }
  case class C28(x: Int) extends Wide
  object C28 { implicit val mongo: MongoFormat[C28] = deriveMongoFormat }
  case class C29(x: Int) extends Wide
  object C29 { implicit val mongo: MongoFormat[C29] = deriveMongoFormat }
  case class C30(x: Int) extends Wide
  object C30 { implicit val mongo: MongoFormat[C30] = deriveMongoFormat }
  case class C31(x: Int) extends Wide
  object C31 { implicit val mongo: MongoFormat[C31] = deriveMongoFormat }
  case class C32(x: Int) extends Wide
  object C32 { implicit val mongo: MongoFormat[C32] = deriveMongoFormat }
  case class C33(x: Int) extends Wide
  object C33 { implicit val mongo: MongoFormat[C33] = deriveMongoFormat }
  case class C34(x: Int) extends Wide
  object C34 { implicit val mongo: MongoFormat[C34] = deriveMongoFormat }
  case class C35(x: Int) extends Wide
  object C35 { implicit val mongo: MongoFormat[C35] = deriveMongoFormat }
  case class C36(x: Int) extends Wide
  object C36 { implicit val mongo: MongoFormat[C36] = deriveMongoFormat }
  case class C37(x: Int) extends Wide
  object C37 { implicit val mongo: MongoFormat[C37] = deriveMongoFormat }
  case class C38(x: Int) extends Wide
  object C38 { implicit val mongo: MongoFormat[C38] = deriveMongoFormat }
  case class C39(x: Int) extends Wide
  object C39 { implicit val mongo: MongoFormat[C39] = deriveMongoFormat }
  case class C40(x: Int) extends Wide
  object C40 { implicit val mongo: MongoFormat[C40] = deriveMongoFormat }
  case class C41(x: Int) extends Wide
  object C41 { implicit val mongo: MongoFormat[C41] = deriveMongoFormat }
  case class C42(x: Int) extends Wide
  object C42 { implicit val mongo: MongoFormat[C42] = deriveMongoFormat }
  case class C43(x: Int) extends Wide
  object C43 { implicit val mongo: MongoFormat[C43] = deriveMongoFormat }
  case class C44(x: Int) extends Wide
  object C44 { implicit val mongo: MongoFormat[C44] = deriveMongoFormat }
  case class C45(x: Int) extends Wide
  object C45 { implicit val mongo: MongoFormat[C45] = deriveMongoFormat }
  case class C46(x: Int) extends Wide
  object C46 { implicit val mongo: MongoFormat[C46] = deriveMongoFormat }
  case class C47(x: Int) extends Wide
  object C47 { implicit val mongo: MongoFormat[C47] = deriveMongoFormat }
  case class C48(x: Int) extends Wide
  object C48 { implicit val mongo: MongoFormat[C48] = deriveMongoFormat }
  case class C49(x: Int) extends Wide
  object C49 { implicit val mongo: MongoFormat[C49] = deriveMongoFormat }
  case class C50(x: Int) extends Wide
  object C50 { implicit val mongo: MongoFormat[C50] = deriveMongoFormat }
  case class C51(x: Int) extends Wide
  object C51 { implicit val mongo: MongoFormat[C51] = deriveMongoFormat }
  case class C52(x: Int) extends Wide
  object C52 { implicit val mongo: MongoFormat[C52] = deriveMongoFormat }
  case class C53(x: Int) extends Wide
  object C53 { implicit val mongo: MongoFormat[C53] = deriveMongoFormat }
  case class C54(x: Int) extends Wide
  object C54 { implicit val mongo: MongoFormat[C54] = deriveMongoFormat }
  case class C55(x: Int) extends Wide
  object C55 { implicit val mongo: MongoFormat[C55] = deriveMongoFormat }
  case class C56(x: Int) extends Wide
  object C56 { implicit val mongo: MongoFormat[C56] = deriveMongoFormat }
  case class C57(x: Int) extends Wide
  object C57 { implicit val mongo: MongoFormat[C57] = deriveMongoFormat }
  case class C58(x: Int) extends Wide
  object C58 { implicit val mongo: MongoFormat[C58] = deriveMongoFormat }
  case class C59(x: Int) extends Wide
  object C59 { implicit val mongo: MongoFormat[C59] = deriveMongoFormat }
  case class C60(x: Int) extends Wide
  object C60 { implicit val mongo: MongoFormat[C60] = deriveMongoFormat }
  case class C61(x: Int) extends Wide
  object C61 { implicit val mongo: MongoFormat[C61] = deriveMongoFormat }
  case class C62(x: Int) extends Wide
  object C62 { implicit val mongo: MongoFormat[C62] = deriveMongoFormat }
  case class C63(x: Int) extends Wide
  object C63 { implicit val mongo: MongoFormat[C63] = deriveMongoFormat }
  case class C64(x: Int) extends Wide
  object C64 { implicit val mongo: MongoFormat[C64] = deriveMongoFormat }
  case class C65(x: Int) extends Wide
  object C65 { implicit val mongo: MongoFormat[C65] = deriveMongoFormat }
  case class C66(x: Int) extends Wide
  object C66 { implicit val mongo: MongoFormat[C66] = deriveMongoFormat }
  case class C67(x: Int) extends Wide
  object C67 { implicit val mongo: MongoFormat[C67] = deriveMongoFormat }
  case class C68(x: Int) extends Wide
  object C68 { implicit val mongo: MongoFormat[C68] = deriveMongoFormat }
  case class C69(x: Int) extends Wide
  object C69 { implicit val mongo: MongoFormat[C69] = deriveMongoFormat }
  case class C70(x: Int) extends Wide
  object C70 { implicit val mongo: MongoFormat[C70] = deriveMongoFormat }
  case class C71(x: Int) extends Wide
  object C71 { implicit val mongo: MongoFormat[C71] = deriveMongoFormat }
  case class C72(x: Int) extends Wide
  object C72 { implicit val mongo: MongoFormat[C72] = deriveMongoFormat }
  case class C73(x: Int) extends Wide
  object C73 { implicit val mongo: MongoFormat[C73] = deriveMongoFormat }
  case class C74(x: Int) extends Wide
  object C74 { implicit val mongo: MongoFormat[C74] = deriveMongoFormat }
  case class C75(x: Int) extends Wide
  object C75 { implicit val mongo: MongoFormat[C75] = deriveMongoFormat }
  case class C76(x: Int) extends Wide
  object C76 { implicit val mongo: MongoFormat[C76] = deriveMongoFormat }
  case class C77(x: Int) extends Wide
  object C77 { implicit val mongo: MongoFormat[C77] = deriveMongoFormat }
  case class C78(x: Int) extends Wide
  object C78 { implicit val mongo: MongoFormat[C78] = deriveMongoFormat }
  case class C79(x: Int) extends Wide
  object C79 { implicit val mongo: MongoFormat[C79] = deriveMongoFormat }
  case class C80(x: Int) extends Wide
  object C80 { implicit val mongo: MongoFormat[C80] = deriveMongoFormat }
  case class C81(x: Int) extends Wide
  object C81 { implicit val mongo: MongoFormat[C81] = deriveMongoFormat }
  case class C82(x: Int) extends Wide
  object C82 { implicit val mongo: MongoFormat[C82] = deriveMongoFormat }
  case class C83(x: Int) extends Wide
  object C83 { implicit val mongo: MongoFormat[C83] = deriveMongoFormat }
  case class C84(x: Int) extends Wide
  object C84 { implicit val mongo: MongoFormat[C84] = deriveMongoFormat }
  case class C85(x: Int) extends Wide
  object C85 { implicit val mongo: MongoFormat[C85] = deriveMongoFormat }
  case class C86(x: Int) extends Wide
  object C86 { implicit val mongo: MongoFormat[C86] = deriveMongoFormat }
  case class C87(x: Int) extends Wide
  object C87 { implicit val mongo: MongoFormat[C87] = deriveMongoFormat }
  case class C88(x: Int) extends Wide
  object C88 { implicit val mongo: MongoFormat[C88] = deriveMongoFormat }
  case class C89(x: Int) extends Wide
  object C89 { implicit val mongo: MongoFormat[C89] = deriveMongoFormat }
  case class C90(x: Int) extends Wide
  object C90 { implicit val mongo: MongoFormat[C90] = deriveMongoFormat }
  case class C91(x: Int) extends Wide
  object C91 { implicit val mongo: MongoFormat[C91] = deriveMongoFormat }
  case class C92(x: Int) extends Wide
  object C92 { implicit val mongo: MongoFormat[C92] = deriveMongoFormat }
  case class C93(x: Int) extends Wide
  object C93 { implicit val mongo: MongoFormat[C93] = deriveMongoFormat }
  case class C94(x: Int) extends Wide
  object C94 { implicit val mongo: MongoFormat[C94] = deriveMongoFormat }
  case class C95(x: Int) extends Wide
  object C95 { implicit val mongo: MongoFormat[C95] = deriveMongoFormat }
  case class C96(x: Int) extends Wide
  object C96 { implicit val mongo: MongoFormat[C96] = deriveMongoFormat }
  case class C97(x: Int) extends Wide
  object C97 { implicit val mongo: MongoFormat[C97] = deriveMongoFormat }
  case class C98(x: Int) extends Wide
  object C98 { implicit val mongo: MongoFormat[C98] = deriveMongoFormat }
  case class C99(x: Int) extends Wide
  object C99 { implicit val mongo: MongoFormat[C99] = deriveMongoFormat }
  case class C100(x: Int) extends Wide
  object C100 { implicit val mongo: MongoFormat[C100] = deriveMongoFormat }
  case class C101(x: Int) extends Wide
  object C101 { implicit val mongo: MongoFormat[C101] = deriveMongoFormat }
  case class C102(x: Int) extends Wide
  object C102 { implicit val mongo: MongoFormat[C102] = deriveMongoFormat }
  case class C103(x: Int) extends Wide
  object C103 { implicit val mongo: MongoFormat[C103] = deriveMongoFormat }
  case class C104(x: Int) extends Wide
  object C104 { implicit val mongo: MongoFormat[C104] = deriveMongoFormat }
  case class C105(x: Int) extends Wide
  object C105 { implicit val mongo: MongoFormat[C105] = deriveMongoFormat }
  case class C106(x: Int) extends Wide
  object C106 { implicit val mongo: MongoFormat[C106] = deriveMongoFormat }
  case class C107(x: Int) extends Wide
  object C107 { implicit val mongo: MongoFormat[C107] = deriveMongoFormat }
  case class C108(x: Int) extends Wide
  object C108 { implicit val mongo: MongoFormat[C108] = deriveMongoFormat }
  case class C109(x: Int) extends Wide
  object C109 { implicit val mongo: MongoFormat[C109] = deriveMongoFormat }
  case class C110(x: Int) extends Wide
  object C110 { implicit val mongo: MongoFormat[C110] = deriveMongoFormat }
  case class C111(x: Int) extends Wide
  object C111 { implicit val mongo: MongoFormat[C111] = deriveMongoFormat }
  case class C112(x: Int) extends Wide
  object C112 { implicit val mongo: MongoFormat[C112] = deriveMongoFormat }
  case class C113(x: Int) extends Wide
  object C113 { implicit val mongo: MongoFormat[C113] = deriveMongoFormat }
  case class C114(x: Int) extends Wide
  object C114 { implicit val mongo: MongoFormat[C114] = deriveMongoFormat }
  case class C115(x: Int) extends Wide
  object C115 { implicit val mongo: MongoFormat[C115] = deriveMongoFormat }
  case class C116(x: Int) extends Wide
  object C116 { implicit val mongo: MongoFormat[C116] = deriveMongoFormat }
  case class C117(x: Int) extends Wide
  object C117 { implicit val mongo: MongoFormat[C117] = deriveMongoFormat }
  case class C118(x: Int) extends Wide
  object C118 { implicit val mongo: MongoFormat[C118] = deriveMongoFormat }
  case class C119(x: Int) extends Wide
  object C119 { implicit val mongo: MongoFormat[C119] = deriveMongoFormat }
  case class C120(x: Int) extends Wide
  object C120 { implicit val mongo: MongoFormat[C120] = deriveMongoFormat }
  case class C121(x: Int) extends Wide
  object C121 { implicit val mongo: MongoFormat[C121] = deriveMongoFormat }
  case class C122(x: Int) extends Wide
  object C122 { implicit val mongo: MongoFormat[C122] = deriveMongoFormat }
  case class C123(x: Int) extends Wide
  object C123 { implicit val mongo: MongoFormat[C123] = deriveMongoFormat }
  case class C124(x: Int) extends Wide
  object C124 { implicit val mongo: MongoFormat[C124] = deriveMongoFormat }
  case class C125(x: Int) extends Wide
  object C125 { implicit val mongo: MongoFormat[C125] = deriveMongoFormat }
  case class C126(x: Int) extends Wide
  object C126 { implicit val mongo: MongoFormat[C126] = deriveMongoFormat }
  case class C127(x: Int) extends Wide
  object C127 { implicit val mongo: MongoFormat[C127] = deriveMongoFormat }
  case class C128(x: Int) extends Wide
  object C128 { implicit val mongo: MongoFormat[C128] = deriveMongoFormat }
  case class C129(x: Int) extends Wide
  object C129 { implicit val mongo: MongoFormat[C129] = deriveMongoFormat }
  case class C130(x: Int) extends Wide
  object C130 { implicit val mongo: MongoFormat[C130] = deriveMongoFormat }
  case class C131(x: Int) extends Wide
  object C131 { implicit val mongo: MongoFormat[C131] = deriveMongoFormat }
  case class C132(x: Int) extends Wide
  object C132 { implicit val mongo: MongoFormat[C132] = deriveMongoFormat }
  case class C133(x: Int) extends Wide
  object C133 { implicit val mongo: MongoFormat[C133] = deriveMongoFormat }
  case class C134(x: Int) extends Wide
  object C134 { implicit val mongo: MongoFormat[C134] = deriveMongoFormat }
  case class C135(x: Int) extends Wide
  object C135 { implicit val mongo: MongoFormat[C135] = deriveMongoFormat }
  case class C136(x: Int) extends Wide
  object C136 { implicit val mongo: MongoFormat[C136] = deriveMongoFormat }
  case class C137(x: Int) extends Wide
  object C137 { implicit val mongo: MongoFormat[C137] = deriveMongoFormat }
  case class C138(x: Int) extends Wide
  object C138 { implicit val mongo: MongoFormat[C138] = deriveMongoFormat }
  case class C139(x: Int) extends Wide
  object C139 { implicit val mongo: MongoFormat[C139] = deriveMongoFormat }
  case class C140(x: Int) extends Wide
  object C140 { implicit val mongo: MongoFormat[C140] = deriveMongoFormat }
  case class C141(x: Int) extends Wide
  object C141 { implicit val mongo: MongoFormat[C141] = deriveMongoFormat }
  case class C142(x: Int) extends Wide
  object C142 { implicit val mongo: MongoFormat[C142] = deriveMongoFormat }
  case class C143(x: Int) extends Wide
  object C143 { implicit val mongo: MongoFormat[C143] = deriveMongoFormat }
  case class C144(x: Int) extends Wide
  object C144 { implicit val mongo: MongoFormat[C144] = deriveMongoFormat }
  case class C145(x: Int) extends Wide
  object C145 { implicit val mongo: MongoFormat[C145] = deriveMongoFormat }
  case class C146(x: Int) extends Wide
  object C146 { implicit val mongo: MongoFormat[C146] = deriveMongoFormat }
  case class C147(x: Int) extends Wide
  object C147 { implicit val mongo: MongoFormat[C147] = deriveMongoFormat }
  case class C148(x: Int) extends Wide
  object C148 { implicit val mongo: MongoFormat[C148] = deriveMongoFormat }
  case class C149(x: Int) extends Wide
  object C149 { implicit val mongo: MongoFormat[C149] = deriveMongoFormat }
  case class C150(x: Int) extends Wide
  object C150 { implicit val mongo: MongoFormat[C150] = deriveMongoFormat }
  case class C151(x: Int) extends Wide
  object C151 { implicit val mongo: MongoFormat[C151] = deriveMongoFormat }
  case class C152(x: Int) extends Wide
  object C152 { implicit val mongo: MongoFormat[C152] = deriveMongoFormat }
  case class C153(x: Int) extends Wide
  object C153 { implicit val mongo: MongoFormat[C153] = deriveMongoFormat }
  case class C154(x: Int) extends Wide
  object C154 { implicit val mongo: MongoFormat[C154] = deriveMongoFormat }
  case class C155(x: Int) extends Wide
  object C155 { implicit val mongo: MongoFormat[C155] = deriveMongoFormat }
  case class C156(x: Int) extends Wide
  object C156 { implicit val mongo: MongoFormat[C156] = deriveMongoFormat }
  case class C157(x: Int) extends Wide
  object C157 { implicit val mongo: MongoFormat[C157] = deriveMongoFormat }
  case class C158(x: Int) extends Wide
  object C158 { implicit val mongo: MongoFormat[C158] = deriveMongoFormat }
  case class C159(x: Int) extends Wide
  object C159 { implicit val mongo: MongoFormat[C159] = deriveMongoFormat }
  case class C160(x: Int) extends Wide
  object C160 { implicit val mongo: MongoFormat[C160] = deriveMongoFormat }
  case class C161(x: Int) extends Wide
  object C161 { implicit val mongo: MongoFormat[C161] = deriveMongoFormat }
  case class C162(x: Int) extends Wide
  object C162 { implicit val mongo: MongoFormat[C162] = deriveMongoFormat }
  case class C163(x: Int) extends Wide
  object C163 { implicit val mongo: MongoFormat[C163] = deriveMongoFormat }
  case class C164(x: Int) extends Wide
  object C164 { implicit val mongo: MongoFormat[C164] = deriveMongoFormat }
  case class C165(x: Int) extends Wide
  object C165 { implicit val mongo: MongoFormat[C165] = deriveMongoFormat }
  case class C166(x: Int) extends Wide
  object C166 { implicit val mongo: MongoFormat[C166] = deriveMongoFormat }
  case class C167(x: Int) extends Wide
  object C167 { implicit val mongo: MongoFormat[C167] = deriveMongoFormat }
  case class C168(x: Int) extends Wide
  object C168 { implicit val mongo: MongoFormat[C168] = deriveMongoFormat }
  case class C169(x: Int) extends Wide
  object C169 { implicit val mongo: MongoFormat[C169] = deriveMongoFormat }
  case class C170(x: Int) extends Wide
  object C170 { implicit val mongo: MongoFormat[C170] = deriveMongoFormat }
  case class C171(x: Int) extends Wide
  object C171 { implicit val mongo: MongoFormat[C171] = deriveMongoFormat }
  case class C172(x: Int) extends Wide
  object C172 { implicit val mongo: MongoFormat[C172] = deriveMongoFormat }
  case class C173(x: Int) extends Wide
  object C173 { implicit val mongo: MongoFormat[C173] = deriveMongoFormat }
  case class C174(x: Int) extends Wide
  object C174 { implicit val mongo: MongoFormat[C174] = deriveMongoFormat }
  case class C175(x: Int) extends Wide
  object C175 { implicit val mongo: MongoFormat[C175] = deriveMongoFormat }
  case class C176(x: Int) extends Wide
  object C176 { implicit val mongo: MongoFormat[C176] = deriveMongoFormat }
  case class C177(x: Int) extends Wide
  object C177 { implicit val mongo: MongoFormat[C177] = deriveMongoFormat }
  case class C178(x: Int) extends Wide
  object C178 { implicit val mongo: MongoFormat[C178] = deriveMongoFormat }
  case class C179(x: Int) extends Wide
  object C179 { implicit val mongo: MongoFormat[C179] = deriveMongoFormat }
  case class C180(x: Int) extends Wide
  object C180 { implicit val mongo: MongoFormat[C180] = deriveMongoFormat }
  case class C181(x: Int) extends Wide
  object C181 { implicit val mongo: MongoFormat[C181] = deriveMongoFormat }
  case class C182(x: Int) extends Wide
  object C182 { implicit val mongo: MongoFormat[C182] = deriveMongoFormat }
  case class C183(x: Int) extends Wide
  object C183 { implicit val mongo: MongoFormat[C183] = deriveMongoFormat }
  case class C184(x: Int) extends Wide
  object C184 { implicit val mongo: MongoFormat[C184] = deriveMongoFormat }
  case class C185(x: Int) extends Wide
  object C185 { implicit val mongo: MongoFormat[C185] = deriveMongoFormat }
  case class C186(x: Int) extends Wide
  object C186 { implicit val mongo: MongoFormat[C186] = deriveMongoFormat }
  case class C187(x: Int) extends Wide
  object C187 { implicit val mongo: MongoFormat[C187] = deriveMongoFormat }
  case class C188(x: Int) extends Wide
  object C188 { implicit val mongo: MongoFormat[C188] = deriveMongoFormat }
  case class C189(x: Int) extends Wide
  object C189 { implicit val mongo: MongoFormat[C189] = deriveMongoFormat }
  case class C190(x: Int) extends Wide
  object C190 { implicit val mongo: MongoFormat[C190] = deriveMongoFormat }
  case class C191(x: Int) extends Wide
  object C191 { implicit val mongo: MongoFormat[C191] = deriveMongoFormat }
  case class C192(x: Int) extends Wide
  object C192 { implicit val mongo: MongoFormat[C192] = deriveMongoFormat }
  case class C193(x: Int) extends Wide
  object C193 { implicit val mongo: MongoFormat[C193] = deriveMongoFormat }
  case class C194(x: Int) extends Wide
  object C194 { implicit val mongo: MongoFormat[C194] = deriveMongoFormat }
  case class C195(x: Int) extends Wide
  object C195 { implicit val mongo: MongoFormat[C195] = deriveMongoFormat }
  case class C196(x: Int) extends Wide
  object C196 { implicit val mongo: MongoFormat[C196] = deriveMongoFormat }
  case class C197(x: Int) extends Wide
  object C197 { implicit val mongo: MongoFormat[C197] = deriveMongoFormat }
  case class C198(x: Int) extends Wide
  object C198 { implicit val mongo: MongoFormat[C198] = deriveMongoFormat }
  case class C199(x: Int) extends Wide
  object C199 { implicit val mongo: MongoFormat[C199] = deriveMongoFormat }
  case class C200(x: Int) extends Wide
  object C200 { implicit val mongo: MongoFormat[C200] = deriveMongoFormat }
  // format: on

  val mongo: MongoFormat[Wide] = mongoTypeSwitch[Wide](
    List(
      sub[C1],
      sub[C2],
      sub[C3],
      sub[C4],
      sub[C5],
      sub[C6],
      sub[C7],
      sub[C8],
      sub[C9],
      sub[C10],
      sub[C11],
      sub[C12],
      sub[C13],
      sub[C14],
      sub[C15],
      sub[C16],
      sub[C17],
      sub[C18],
      sub[C19],
      sub[C20],
      sub[C21],
      sub[C22],
      sub[C23],
      sub[C24],
      sub[C25],
      sub[C26],
      sub[C27],
      sub[C28],
      sub[C29],
      sub[C30],
      sub[C31],
      sub[C32],
      sub[C33],
      sub[C34],
      sub[C35],
      sub[C36],
      sub[C37],
      sub[C38],
      sub[C39],
      sub[C40],
      sub[C41],
      sub[C42],
      sub[C43],
      sub[C44],
      sub[C45],
      sub[C46],
      sub[C47],
      sub[C48],
      sub[C49],
      sub[C50],
      sub[C51],
      sub[C52],
      sub[C53],
      sub[C54],
      sub[C55],
      sub[C56],
      sub[C57],
      sub[C58],
      sub[C59],
      sub[C60],
      sub[C61],
      sub[C62],
      sub[C63],
      sub[C64],
      sub[C65],
      sub[C66],
      sub[C67],
      sub[C68],
      sub[C69],
      sub[C70],
      sub[C71],
      sub[C72],
      sub[C73],
      sub[C74],
      sub[C75],
      sub[C76],
      sub[C77],
      sub[C78],
      sub[C79],
      sub[C80],
      sub[C81],
      sub[C82],
      sub[C83],
      sub[C84],
      sub[C85],
      sub[C86],
      sub[C87],
      sub[C88],
      sub[C89],
      sub[C90],
      sub[C91],
      sub[C92],
      sub[C93],
      sub[C94],
      sub[C95],
      sub[C96],
      sub[C97],
      sub[C98],
      sub[C99],
      sub[C100],
      sub[C101],
      sub[C102],
      sub[C103],
      sub[C104],
      sub[C105],
      sub[C106],
      sub[C107],
      sub[C108],
      sub[C109],
      sub[C110],
      sub[C111],
      sub[C112],
      sub[C113],
      sub[C114],
      sub[C115],
      sub[C116],
      sub[C117],
      sub[C118],
      sub[C119],
      sub[C120],
      sub[C121],
      sub[C122],
      sub[C123],
      sub[C124],
      sub[C125],
      sub[C126],
      sub[C127],
      sub[C128],
      sub[C129],
      sub[C130],
      sub[C131],
      sub[C132],
      sub[C133],
      sub[C134],
      sub[C135],
      sub[C136],
      sub[C137],
      sub[C138],
      sub[C139],
      sub[C140],
      sub[C141],
      sub[C142],
      sub[C143],
      sub[C144],
      sub[C145],
      sub[C146],
      sub[C147],
      sub[C148],
      sub[C149],
      sub[C150],
      sub[C151],
      sub[C152],
      sub[C153],
      sub[C154],
      sub[C155],
      sub[C156],
      sub[C157],
      sub[C158],
      sub[C159],
      sub[C160],
      sub[C161],
      sub[C162],
      sub[C163],
      sub[C164],
      sub[C165],
      sub[C166],
      sub[C167],
      sub[C168],
      sub[C169],
      sub[C170],
      sub[C171],
      sub[C172],
      sub[C173],
      sub[C174],
      sub[C175],
      sub[C176],
      sub[C177],
      sub[C178],
      sub[C179],
      sub[C180],
      sub[C181],
      sub[C182],
      sub[C183],
      sub[C184],
      sub[C185],
      sub[C186],
      sub[C187],
      sub[C188],
      sub[C189],
      sub[C190],
      sub[C191],
      sub[C192],
      sub[C193],
      sub[C194],
      sub[C195],
      sub[C196],
      sub[C197],
      sub[C198],
      sub[C199],
      sub[C200]
    ))

  val values: List[Wide] = List(
    C1(1),
    C2(2),
    C3(3),
    C4(4),
    C5(5),
    C6(6),
    C7(7),
    C8(8),
    C9(9),
    C10(10),
    C11(11),
    C12(12),
    C13(13),
    C14(14),
    C15(15),
    C16(16),
    C17(17),
    C18(18),
    C19(19),
    C20(20),
    C21(21),
    C22(22),
    C23(23),
    C24(24),
    C25(25),
    C26(26),
    C27(27),
    C28(28),
    C29(29),
    C30(30),
    C31(31),
    C32(32),
    C33(33),
    C34(34),
    C35(35),
    C36(36),
    C37(37),
    C38(38),
    C39(39),
    C40(40),
    C41(41),
    C42(42),
    C43(43),
    C44(44),
    C45(45),
    C46(46),
    C47(47),
    C48(48),
    C49(49),
    C50(50),
    C51(51),
    C52(52),
    C53(53),
    C54(54),
    C55(55),
    C56(56),
    C57(57),
    C58(58),
    C59(59),
    C60(60),
    C61(61),
    C62(62),
    C63(63),
    C64(64),
    C65(65),
    C66(66),
    C67(67),
    C68(68),
    C69(69),
    C70(70),
    C71(71),
    C72(72),
    C73(73),
    C74(74),
    C75(75),
    C76(76),
    C77(77),
    C78(78),
    C79(79),
    C80(80),
    C81(81),
    C82(82),
    C83(83),
    C84(84),
    C85(85),
    C86(86),
    C87(87),
    C88(88),
    C89(89),
    C90(90),
    C91(91),
    C92(92),
    C93(93),
    C94(94),
    C95(95),
    C96(96),
    C97(97),
    C98(98),
    C99(99),
    C100(100),
    C101(101),
    C102(102),
    C103(103),
    C104(104),
    C105(105),
    C106(106),
    C107(107),
    C108(108),
    C109(109),
    C110(110),
    C111(111),
    C112(112),
    C113(113),
    C114(114),
    C115(115),
    C116(116),
    C117(117),
    C118(118),
    C119(119),
    C120(120),
    C121(121),
    C122(122),
    C123(123),
    C124(124),
    C125(125),
    C126(126),
    C127(127),
    C128(128),
    C129(129),
    C130(130),
    C131(131),
    C132(132),
    C133(133),
    C134(134),
    C135(135),
    C136(136),
    C137(137),
    C138(138),
    C139(139),
    C140(140),
    C141(141),
    C142(142),
    C143(143),
    C144(144),
    C145(145),
    C146(146),
    C147(147),
    C148(148),
    C149(149),
    C150(150),
    C151(151),
    C152(152),
    C153(153),
    C154(154),
    C155(155),
    C156(156),
    C157(157),
    C158(158),
    C159(159),
    C160(160),
    C161(161),
    C162(162),
    C163(163),
    C164(164),
    C165(165),
    C166(166),
    C167(167),
    C168(168),
    C169(169),
    C170(170),
    C171(171),
    C172(172),
    C173(173),
    C174(174),
    C175(175),
    C176(176),
    C177(177),
    C178(178),
    C179(179),
    C180(180),
    C181(181),
    C182(182),
    C183(183),
    C184(184),
    C185(185),
    C186(186),
    C187(187),
    C188(188),
    C189(189),
    C190(190),
    C191(191),
    C192(192),
    C193(193),
    C194(194),
    C195(195),
    C196(196),
    C197(197),
    C198(198),
    C199(199),
    C200(200)
  )
}
