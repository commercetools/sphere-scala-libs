package io.sphere.json.generic

import io.sphere.json.JSON
import io.sphere.util.test._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** A sealed trait far wider than either version used to support: Scala 2 was capped at ~125
  * subtypes by the JVM method-parameter limit (each subtype was a type parameter carrying two
  * implicit params), Scala 3 at ~25 by `-Xmax-inlines`. Both caps went away when the subtypes moved
  * from type parameters to a value-level selector list, and this spec is what keeps them away - if
  * either ceiling comes back, this stops compiling.
  *
  * The structure is not arbitrary. Scala 3's `sub` is `inline`, so it expands into its calling
  * method and the binding limit is now the JVM's 64KB per method - measured at ~52 `sub` calls.
  * Hence the selectors are built in chunks of 25. Each subtype's instance also lives in its own
  * companion, so the 200 `deriveJSON` expansions land in 200 tiny class initialisers instead of one
  * huge one; Scala 2 needs those companions regardless, having no automatic derivation.
  */
class WideTypeSwitchSpec extends AnyWordSpec with Matchers {
  import WideTypeSwitchSpec._

  "jsonTypeSwitch" must {
    "handle 200 subtypes" in {
      json.subTypeNames.size must be(200)
      json.subTypeNames.distinct.size must be(200)

      values.size must be(200)
      values.foreach(value => json.read(json.write(value)).expectValid must be(value))
    }
  }
}

object WideTypeSwitchSpec {
  sealed trait Wide

  // format: off
  case class C1(x: Int) extends Wide
  object C1 { implicit val json: JSON[C1] = deriveJSON[C1] }
  case class C2(x: Int) extends Wide
  object C2 { implicit val json: JSON[C2] = deriveJSON[C2] }
  case class C3(x: Int) extends Wide
  object C3 { implicit val json: JSON[C3] = deriveJSON[C3] }
  case class C4(x: Int) extends Wide
  object C4 { implicit val json: JSON[C4] = deriveJSON[C4] }
  case class C5(x: Int) extends Wide
  object C5 { implicit val json: JSON[C5] = deriveJSON[C5] }
  case class C6(x: Int) extends Wide
  object C6 { implicit val json: JSON[C6] = deriveJSON[C6] }
  case class C7(x: Int) extends Wide
  object C7 { implicit val json: JSON[C7] = deriveJSON[C7] }
  case class C8(x: Int) extends Wide
  object C8 { implicit val json: JSON[C8] = deriveJSON[C8] }
  case class C9(x: Int) extends Wide
  object C9 { implicit val json: JSON[C9] = deriveJSON[C9] }
  case class C10(x: Int) extends Wide
  object C10 { implicit val json: JSON[C10] = deriveJSON[C10] }
  case class C11(x: Int) extends Wide
  object C11 { implicit val json: JSON[C11] = deriveJSON[C11] }
  case class C12(x: Int) extends Wide
  object C12 { implicit val json: JSON[C12] = deriveJSON[C12] }
  case class C13(x: Int) extends Wide
  object C13 { implicit val json: JSON[C13] = deriveJSON[C13] }
  case class C14(x: Int) extends Wide
  object C14 { implicit val json: JSON[C14] = deriveJSON[C14] }
  case class C15(x: Int) extends Wide
  object C15 { implicit val json: JSON[C15] = deriveJSON[C15] }
  case class C16(x: Int) extends Wide
  object C16 { implicit val json: JSON[C16] = deriveJSON[C16] }
  case class C17(x: Int) extends Wide
  object C17 { implicit val json: JSON[C17] = deriveJSON[C17] }
  case class C18(x: Int) extends Wide
  object C18 { implicit val json: JSON[C18] = deriveJSON[C18] }
  case class C19(x: Int) extends Wide
  object C19 { implicit val json: JSON[C19] = deriveJSON[C19] }
  case class C20(x: Int) extends Wide
  object C20 { implicit val json: JSON[C20] = deriveJSON[C20] }
  case class C21(x: Int) extends Wide
  object C21 { implicit val json: JSON[C21] = deriveJSON[C21] }
  case class C22(x: Int) extends Wide
  object C22 { implicit val json: JSON[C22] = deriveJSON[C22] }
  case class C23(x: Int) extends Wide
  object C23 { implicit val json: JSON[C23] = deriveJSON[C23] }
  case class C24(x: Int) extends Wide
  object C24 { implicit val json: JSON[C24] = deriveJSON[C24] }
  case class C25(x: Int) extends Wide
  object C25 { implicit val json: JSON[C25] = deriveJSON[C25] }
  case class C26(x: Int) extends Wide
  object C26 { implicit val json: JSON[C26] = deriveJSON[C26] }
  case class C27(x: Int) extends Wide
  object C27 { implicit val json: JSON[C27] = deriveJSON[C27] }
  case class C28(x: Int) extends Wide
  object C28 { implicit val json: JSON[C28] = deriveJSON[C28] }
  case class C29(x: Int) extends Wide
  object C29 { implicit val json: JSON[C29] = deriveJSON[C29] }
  case class C30(x: Int) extends Wide
  object C30 { implicit val json: JSON[C30] = deriveJSON[C30] }
  case class C31(x: Int) extends Wide
  object C31 { implicit val json: JSON[C31] = deriveJSON[C31] }
  case class C32(x: Int) extends Wide
  object C32 { implicit val json: JSON[C32] = deriveJSON[C32] }
  case class C33(x: Int) extends Wide
  object C33 { implicit val json: JSON[C33] = deriveJSON[C33] }
  case class C34(x: Int) extends Wide
  object C34 { implicit val json: JSON[C34] = deriveJSON[C34] }
  case class C35(x: Int) extends Wide
  object C35 { implicit val json: JSON[C35] = deriveJSON[C35] }
  case class C36(x: Int) extends Wide
  object C36 { implicit val json: JSON[C36] = deriveJSON[C36] }
  case class C37(x: Int) extends Wide
  object C37 { implicit val json: JSON[C37] = deriveJSON[C37] }
  case class C38(x: Int) extends Wide
  object C38 { implicit val json: JSON[C38] = deriveJSON[C38] }
  case class C39(x: Int) extends Wide
  object C39 { implicit val json: JSON[C39] = deriveJSON[C39] }
  case class C40(x: Int) extends Wide
  object C40 { implicit val json: JSON[C40] = deriveJSON[C40] }
  case class C41(x: Int) extends Wide
  object C41 { implicit val json: JSON[C41] = deriveJSON[C41] }
  case class C42(x: Int) extends Wide
  object C42 { implicit val json: JSON[C42] = deriveJSON[C42] }
  case class C43(x: Int) extends Wide
  object C43 { implicit val json: JSON[C43] = deriveJSON[C43] }
  case class C44(x: Int) extends Wide
  object C44 { implicit val json: JSON[C44] = deriveJSON[C44] }
  case class C45(x: Int) extends Wide
  object C45 { implicit val json: JSON[C45] = deriveJSON[C45] }
  case class C46(x: Int) extends Wide
  object C46 { implicit val json: JSON[C46] = deriveJSON[C46] }
  case class C47(x: Int) extends Wide
  object C47 { implicit val json: JSON[C47] = deriveJSON[C47] }
  case class C48(x: Int) extends Wide
  object C48 { implicit val json: JSON[C48] = deriveJSON[C48] }
  case class C49(x: Int) extends Wide
  object C49 { implicit val json: JSON[C49] = deriveJSON[C49] }
  case class C50(x: Int) extends Wide
  object C50 { implicit val json: JSON[C50] = deriveJSON[C50] }
  case class C51(x: Int) extends Wide
  object C51 { implicit val json: JSON[C51] = deriveJSON[C51] }
  case class C52(x: Int) extends Wide
  object C52 { implicit val json: JSON[C52] = deriveJSON[C52] }
  case class C53(x: Int) extends Wide
  object C53 { implicit val json: JSON[C53] = deriveJSON[C53] }
  case class C54(x: Int) extends Wide
  object C54 { implicit val json: JSON[C54] = deriveJSON[C54] }
  case class C55(x: Int) extends Wide
  object C55 { implicit val json: JSON[C55] = deriveJSON[C55] }
  case class C56(x: Int) extends Wide
  object C56 { implicit val json: JSON[C56] = deriveJSON[C56] }
  case class C57(x: Int) extends Wide
  object C57 { implicit val json: JSON[C57] = deriveJSON[C57] }
  case class C58(x: Int) extends Wide
  object C58 { implicit val json: JSON[C58] = deriveJSON[C58] }
  case class C59(x: Int) extends Wide
  object C59 { implicit val json: JSON[C59] = deriveJSON[C59] }
  case class C60(x: Int) extends Wide
  object C60 { implicit val json: JSON[C60] = deriveJSON[C60] }
  case class C61(x: Int) extends Wide
  object C61 { implicit val json: JSON[C61] = deriveJSON[C61] }
  case class C62(x: Int) extends Wide
  object C62 { implicit val json: JSON[C62] = deriveJSON[C62] }
  case class C63(x: Int) extends Wide
  object C63 { implicit val json: JSON[C63] = deriveJSON[C63] }
  case class C64(x: Int) extends Wide
  object C64 { implicit val json: JSON[C64] = deriveJSON[C64] }
  case class C65(x: Int) extends Wide
  object C65 { implicit val json: JSON[C65] = deriveJSON[C65] }
  case class C66(x: Int) extends Wide
  object C66 { implicit val json: JSON[C66] = deriveJSON[C66] }
  case class C67(x: Int) extends Wide
  object C67 { implicit val json: JSON[C67] = deriveJSON[C67] }
  case class C68(x: Int) extends Wide
  object C68 { implicit val json: JSON[C68] = deriveJSON[C68] }
  case class C69(x: Int) extends Wide
  object C69 { implicit val json: JSON[C69] = deriveJSON[C69] }
  case class C70(x: Int) extends Wide
  object C70 { implicit val json: JSON[C70] = deriveJSON[C70] }
  case class C71(x: Int) extends Wide
  object C71 { implicit val json: JSON[C71] = deriveJSON[C71] }
  case class C72(x: Int) extends Wide
  object C72 { implicit val json: JSON[C72] = deriveJSON[C72] }
  case class C73(x: Int) extends Wide
  object C73 { implicit val json: JSON[C73] = deriveJSON[C73] }
  case class C74(x: Int) extends Wide
  object C74 { implicit val json: JSON[C74] = deriveJSON[C74] }
  case class C75(x: Int) extends Wide
  object C75 { implicit val json: JSON[C75] = deriveJSON[C75] }
  case class C76(x: Int) extends Wide
  object C76 { implicit val json: JSON[C76] = deriveJSON[C76] }
  case class C77(x: Int) extends Wide
  object C77 { implicit val json: JSON[C77] = deriveJSON[C77] }
  case class C78(x: Int) extends Wide
  object C78 { implicit val json: JSON[C78] = deriveJSON[C78] }
  case class C79(x: Int) extends Wide
  object C79 { implicit val json: JSON[C79] = deriveJSON[C79] }
  case class C80(x: Int) extends Wide
  object C80 { implicit val json: JSON[C80] = deriveJSON[C80] }
  case class C81(x: Int) extends Wide
  object C81 { implicit val json: JSON[C81] = deriveJSON[C81] }
  case class C82(x: Int) extends Wide
  object C82 { implicit val json: JSON[C82] = deriveJSON[C82] }
  case class C83(x: Int) extends Wide
  object C83 { implicit val json: JSON[C83] = deriveJSON[C83] }
  case class C84(x: Int) extends Wide
  object C84 { implicit val json: JSON[C84] = deriveJSON[C84] }
  case class C85(x: Int) extends Wide
  object C85 { implicit val json: JSON[C85] = deriveJSON[C85] }
  case class C86(x: Int) extends Wide
  object C86 { implicit val json: JSON[C86] = deriveJSON[C86] }
  case class C87(x: Int) extends Wide
  object C87 { implicit val json: JSON[C87] = deriveJSON[C87] }
  case class C88(x: Int) extends Wide
  object C88 { implicit val json: JSON[C88] = deriveJSON[C88] }
  case class C89(x: Int) extends Wide
  object C89 { implicit val json: JSON[C89] = deriveJSON[C89] }
  case class C90(x: Int) extends Wide
  object C90 { implicit val json: JSON[C90] = deriveJSON[C90] }
  case class C91(x: Int) extends Wide
  object C91 { implicit val json: JSON[C91] = deriveJSON[C91] }
  case class C92(x: Int) extends Wide
  object C92 { implicit val json: JSON[C92] = deriveJSON[C92] }
  case class C93(x: Int) extends Wide
  object C93 { implicit val json: JSON[C93] = deriveJSON[C93] }
  case class C94(x: Int) extends Wide
  object C94 { implicit val json: JSON[C94] = deriveJSON[C94] }
  case class C95(x: Int) extends Wide
  object C95 { implicit val json: JSON[C95] = deriveJSON[C95] }
  case class C96(x: Int) extends Wide
  object C96 { implicit val json: JSON[C96] = deriveJSON[C96] }
  case class C97(x: Int) extends Wide
  object C97 { implicit val json: JSON[C97] = deriveJSON[C97] }
  case class C98(x: Int) extends Wide
  object C98 { implicit val json: JSON[C98] = deriveJSON[C98] }
  case class C99(x: Int) extends Wide
  object C99 { implicit val json: JSON[C99] = deriveJSON[C99] }
  case class C100(x: Int) extends Wide
  object C100 { implicit val json: JSON[C100] = deriveJSON[C100] }
  case class C101(x: Int) extends Wide
  object C101 { implicit val json: JSON[C101] = deriveJSON[C101] }
  case class C102(x: Int) extends Wide
  object C102 { implicit val json: JSON[C102] = deriveJSON[C102] }
  case class C103(x: Int) extends Wide
  object C103 { implicit val json: JSON[C103] = deriveJSON[C103] }
  case class C104(x: Int) extends Wide
  object C104 { implicit val json: JSON[C104] = deriveJSON[C104] }
  case class C105(x: Int) extends Wide
  object C105 { implicit val json: JSON[C105] = deriveJSON[C105] }
  case class C106(x: Int) extends Wide
  object C106 { implicit val json: JSON[C106] = deriveJSON[C106] }
  case class C107(x: Int) extends Wide
  object C107 { implicit val json: JSON[C107] = deriveJSON[C107] }
  case class C108(x: Int) extends Wide
  object C108 { implicit val json: JSON[C108] = deriveJSON[C108] }
  case class C109(x: Int) extends Wide
  object C109 { implicit val json: JSON[C109] = deriveJSON[C109] }
  case class C110(x: Int) extends Wide
  object C110 { implicit val json: JSON[C110] = deriveJSON[C110] }
  case class C111(x: Int) extends Wide
  object C111 { implicit val json: JSON[C111] = deriveJSON[C111] }
  case class C112(x: Int) extends Wide
  object C112 { implicit val json: JSON[C112] = deriveJSON[C112] }
  case class C113(x: Int) extends Wide
  object C113 { implicit val json: JSON[C113] = deriveJSON[C113] }
  case class C114(x: Int) extends Wide
  object C114 { implicit val json: JSON[C114] = deriveJSON[C114] }
  case class C115(x: Int) extends Wide
  object C115 { implicit val json: JSON[C115] = deriveJSON[C115] }
  case class C116(x: Int) extends Wide
  object C116 { implicit val json: JSON[C116] = deriveJSON[C116] }
  case class C117(x: Int) extends Wide
  object C117 { implicit val json: JSON[C117] = deriveJSON[C117] }
  case class C118(x: Int) extends Wide
  object C118 { implicit val json: JSON[C118] = deriveJSON[C118] }
  case class C119(x: Int) extends Wide
  object C119 { implicit val json: JSON[C119] = deriveJSON[C119] }
  case class C120(x: Int) extends Wide
  object C120 { implicit val json: JSON[C120] = deriveJSON[C120] }
  case class C121(x: Int) extends Wide
  object C121 { implicit val json: JSON[C121] = deriveJSON[C121] }
  case class C122(x: Int) extends Wide
  object C122 { implicit val json: JSON[C122] = deriveJSON[C122] }
  case class C123(x: Int) extends Wide
  object C123 { implicit val json: JSON[C123] = deriveJSON[C123] }
  case class C124(x: Int) extends Wide
  object C124 { implicit val json: JSON[C124] = deriveJSON[C124] }
  case class C125(x: Int) extends Wide
  object C125 { implicit val json: JSON[C125] = deriveJSON[C125] }
  case class C126(x: Int) extends Wide
  object C126 { implicit val json: JSON[C126] = deriveJSON[C126] }
  case class C127(x: Int) extends Wide
  object C127 { implicit val json: JSON[C127] = deriveJSON[C127] }
  case class C128(x: Int) extends Wide
  object C128 { implicit val json: JSON[C128] = deriveJSON[C128] }
  case class C129(x: Int) extends Wide
  object C129 { implicit val json: JSON[C129] = deriveJSON[C129] }
  case class C130(x: Int) extends Wide
  object C130 { implicit val json: JSON[C130] = deriveJSON[C130] }
  case class C131(x: Int) extends Wide
  object C131 { implicit val json: JSON[C131] = deriveJSON[C131] }
  case class C132(x: Int) extends Wide
  object C132 { implicit val json: JSON[C132] = deriveJSON[C132] }
  case class C133(x: Int) extends Wide
  object C133 { implicit val json: JSON[C133] = deriveJSON[C133] }
  case class C134(x: Int) extends Wide
  object C134 { implicit val json: JSON[C134] = deriveJSON[C134] }
  case class C135(x: Int) extends Wide
  object C135 { implicit val json: JSON[C135] = deriveJSON[C135] }
  case class C136(x: Int) extends Wide
  object C136 { implicit val json: JSON[C136] = deriveJSON[C136] }
  case class C137(x: Int) extends Wide
  object C137 { implicit val json: JSON[C137] = deriveJSON[C137] }
  case class C138(x: Int) extends Wide
  object C138 { implicit val json: JSON[C138] = deriveJSON[C138] }
  case class C139(x: Int) extends Wide
  object C139 { implicit val json: JSON[C139] = deriveJSON[C139] }
  case class C140(x: Int) extends Wide
  object C140 { implicit val json: JSON[C140] = deriveJSON[C140] }
  case class C141(x: Int) extends Wide
  object C141 { implicit val json: JSON[C141] = deriveJSON[C141] }
  case class C142(x: Int) extends Wide
  object C142 { implicit val json: JSON[C142] = deriveJSON[C142] }
  case class C143(x: Int) extends Wide
  object C143 { implicit val json: JSON[C143] = deriveJSON[C143] }
  case class C144(x: Int) extends Wide
  object C144 { implicit val json: JSON[C144] = deriveJSON[C144] }
  case class C145(x: Int) extends Wide
  object C145 { implicit val json: JSON[C145] = deriveJSON[C145] }
  case class C146(x: Int) extends Wide
  object C146 { implicit val json: JSON[C146] = deriveJSON[C146] }
  case class C147(x: Int) extends Wide
  object C147 { implicit val json: JSON[C147] = deriveJSON[C147] }
  case class C148(x: Int) extends Wide
  object C148 { implicit val json: JSON[C148] = deriveJSON[C148] }
  case class C149(x: Int) extends Wide
  object C149 { implicit val json: JSON[C149] = deriveJSON[C149] }
  case class C150(x: Int) extends Wide
  object C150 { implicit val json: JSON[C150] = deriveJSON[C150] }
  case class C151(x: Int) extends Wide
  object C151 { implicit val json: JSON[C151] = deriveJSON[C151] }
  case class C152(x: Int) extends Wide
  object C152 { implicit val json: JSON[C152] = deriveJSON[C152] }
  case class C153(x: Int) extends Wide
  object C153 { implicit val json: JSON[C153] = deriveJSON[C153] }
  case class C154(x: Int) extends Wide
  object C154 { implicit val json: JSON[C154] = deriveJSON[C154] }
  case class C155(x: Int) extends Wide
  object C155 { implicit val json: JSON[C155] = deriveJSON[C155] }
  case class C156(x: Int) extends Wide
  object C156 { implicit val json: JSON[C156] = deriveJSON[C156] }
  case class C157(x: Int) extends Wide
  object C157 { implicit val json: JSON[C157] = deriveJSON[C157] }
  case class C158(x: Int) extends Wide
  object C158 { implicit val json: JSON[C158] = deriveJSON[C158] }
  case class C159(x: Int) extends Wide
  object C159 { implicit val json: JSON[C159] = deriveJSON[C159] }
  case class C160(x: Int) extends Wide
  object C160 { implicit val json: JSON[C160] = deriveJSON[C160] }
  case class C161(x: Int) extends Wide
  object C161 { implicit val json: JSON[C161] = deriveJSON[C161] }
  case class C162(x: Int) extends Wide
  object C162 { implicit val json: JSON[C162] = deriveJSON[C162] }
  case class C163(x: Int) extends Wide
  object C163 { implicit val json: JSON[C163] = deriveJSON[C163] }
  case class C164(x: Int) extends Wide
  object C164 { implicit val json: JSON[C164] = deriveJSON[C164] }
  case class C165(x: Int) extends Wide
  object C165 { implicit val json: JSON[C165] = deriveJSON[C165] }
  case class C166(x: Int) extends Wide
  object C166 { implicit val json: JSON[C166] = deriveJSON[C166] }
  case class C167(x: Int) extends Wide
  object C167 { implicit val json: JSON[C167] = deriveJSON[C167] }
  case class C168(x: Int) extends Wide
  object C168 { implicit val json: JSON[C168] = deriveJSON[C168] }
  case class C169(x: Int) extends Wide
  object C169 { implicit val json: JSON[C169] = deriveJSON[C169] }
  case class C170(x: Int) extends Wide
  object C170 { implicit val json: JSON[C170] = deriveJSON[C170] }
  case class C171(x: Int) extends Wide
  object C171 { implicit val json: JSON[C171] = deriveJSON[C171] }
  case class C172(x: Int) extends Wide
  object C172 { implicit val json: JSON[C172] = deriveJSON[C172] }
  case class C173(x: Int) extends Wide
  object C173 { implicit val json: JSON[C173] = deriveJSON[C173] }
  case class C174(x: Int) extends Wide
  object C174 { implicit val json: JSON[C174] = deriveJSON[C174] }
  case class C175(x: Int) extends Wide
  object C175 { implicit val json: JSON[C175] = deriveJSON[C175] }
  case class C176(x: Int) extends Wide
  object C176 { implicit val json: JSON[C176] = deriveJSON[C176] }
  case class C177(x: Int) extends Wide
  object C177 { implicit val json: JSON[C177] = deriveJSON[C177] }
  case class C178(x: Int) extends Wide
  object C178 { implicit val json: JSON[C178] = deriveJSON[C178] }
  case class C179(x: Int) extends Wide
  object C179 { implicit val json: JSON[C179] = deriveJSON[C179] }
  case class C180(x: Int) extends Wide
  object C180 { implicit val json: JSON[C180] = deriveJSON[C180] }
  case class C181(x: Int) extends Wide
  object C181 { implicit val json: JSON[C181] = deriveJSON[C181] }
  case class C182(x: Int) extends Wide
  object C182 { implicit val json: JSON[C182] = deriveJSON[C182] }
  case class C183(x: Int) extends Wide
  object C183 { implicit val json: JSON[C183] = deriveJSON[C183] }
  case class C184(x: Int) extends Wide
  object C184 { implicit val json: JSON[C184] = deriveJSON[C184] }
  case class C185(x: Int) extends Wide
  object C185 { implicit val json: JSON[C185] = deriveJSON[C185] }
  case class C186(x: Int) extends Wide
  object C186 { implicit val json: JSON[C186] = deriveJSON[C186] }
  case class C187(x: Int) extends Wide
  object C187 { implicit val json: JSON[C187] = deriveJSON[C187] }
  case class C188(x: Int) extends Wide
  object C188 { implicit val json: JSON[C188] = deriveJSON[C188] }
  case class C189(x: Int) extends Wide
  object C189 { implicit val json: JSON[C189] = deriveJSON[C189] }
  case class C190(x: Int) extends Wide
  object C190 { implicit val json: JSON[C190] = deriveJSON[C190] }
  case class C191(x: Int) extends Wide
  object C191 { implicit val json: JSON[C191] = deriveJSON[C191] }
  case class C192(x: Int) extends Wide
  object C192 { implicit val json: JSON[C192] = deriveJSON[C192] }
  case class C193(x: Int) extends Wide
  object C193 { implicit val json: JSON[C193] = deriveJSON[C193] }
  case class C194(x: Int) extends Wide
  object C194 { implicit val json: JSON[C194] = deriveJSON[C194] }
  case class C195(x: Int) extends Wide
  object C195 { implicit val json: JSON[C195] = deriveJSON[C195] }
  case class C196(x: Int) extends Wide
  object C196 { implicit val json: JSON[C196] = deriveJSON[C196] }
  case class C197(x: Int) extends Wide
  object C197 { implicit val json: JSON[C197] = deriveJSON[C197] }
  case class C198(x: Int) extends Wide
  object C198 { implicit val json: JSON[C198] = deriveJSON[C198] }
  case class C199(x: Int) extends Wide
  object C199 { implicit val json: JSON[C199] = deriveJSON[C199] }
  case class C200(x: Int) extends Wide
  object C200 { implicit val json: JSON[C200] = deriveJSON[C200] }
  // format: on

  private def subs1 = List(
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
    sub[C25]
  )

  private def subs2 = List(
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
    sub[C50]
  )

  private def subs3 = List(
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
    sub[C75]
  )

  private def subs4 = List(
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
    sub[C100]
  )

  private def subs5 = List(
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
    sub[C125]
  )

  private def subs6 = List(
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
    sub[C150]
  )

  private def subs7 = List(
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
    sub[C175]
  )

  private def subs8 = List(
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
  )

  private def values1: List[Wide] = List(
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
    C25(25)
  )

  private def values2: List[Wide] = List(
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
    C50(50)
  )

  private def values3: List[Wide] = List(
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
    C75(75)
  )

  private def values4: List[Wide] = List(
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
    C100(100)
  )

  private def values5: List[Wide] = List(
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
    C125(125)
  )

  private def values6: List[Wide] = List(
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
    C150(150)
  )

  private def values7: List[Wide] = List(
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
    C175(175)
  )

  private def values8: List[Wide] = List(
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

  val values: List[Wide] =
    values1 ::: values2 ::: values3 ::: values4 ::: values5 ::: values6 ::: values7 ::: values8

  val json: JSON[Wide] =
    jsonTypeSwitch[Wide](
      subs1 ::: subs2 ::: subs3 ::: subs4 ::: subs5 ::: subs6 ::: subs7 ::: subs8
    )
}
