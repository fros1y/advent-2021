import collection.{IndexedSeq, Iterable, Seq}
import parsley.Parsley, Parsley._
import parsley.errors._
import parsley.lift._
import parsley.character.{
  char,
  string,
  digit,
  anyChar,
  letter,
  whitespace,
  newline,
  space,
  satisfy
}
import parsley.implicits.character.{charLift, stringLift}
import parsley.combinator._
import parsley.{Result, Success, Failure}
import parsley.debug.DebugCombinators

import parsley.token.{LanguageDef, Lexer, Predicate, Parser}
import parsley.character.isWhitespace

@main
def solve_problem16 = {
  val sample_input1 = "A0016C880162017C3686B18A3D4780"
  // "C0015000016115A2E0802F182340"
  //"620080001611562C8802118E34"
  //"8A004A801A8002F478"
//    "38006F45291200"
  //  "EE00D40C823060"
  val binary = Problem16.toBinary(Problem16.input)
//  println(binary)
  //println(binary.length)

  val packet = Problem16.parse(binary)
//  println(packet)
  println(Problem16.addVersions(packet))
}

object Problem16 {

  def addVersions(p: Packet): Int = p match {
    case Packet.Literal(version, _) => Integer.parseInt(version, 2)
    case Packet.Operator(version, _, contents) =>
      Integer.parseInt(version, 2) + contents.map(addVersions).sum
  }

  val literal = string("100").debug("Literal type")
  val command = repeat(3, anyChar).debug("command type")
  val p_type = (literal <|> command).debug("type")
  val p_version = repeat(3, anyChar).debug("version")

  val value_bits = repeat(4, anyChar).debug("value_bits")
  val continue_bit = char('1').debug("continue_bit")
  val end_bit = char('0').debug("end_bit")
  val value_chunks = (continue_chunk <|> end_chunk).debug("value_chunks")
  val continue_chunk: Parsley[List[Char]] =
    (continue_bit *> lift2(
      (x: List[Char], y: List[Char]) => x ::: y,
      value_bits,
      value_chunks
    ))
      .debug("continue_chunk")
  val end_chunk = (end_bit *> value_bits).debug("end_chunk")
  val end_padding = many(char('0')).debug("end_padding")

  val packet: Parsley[Packet] = (literal_packet <|> op_packet)
  val packets: Parsley[Packet] = packet <* end_padding

  enum LengthMode:
    case BitLength(bits: Int)
    case PacketLength(packets: Int)

  val total_length_mode =
    char('0') *> repeat(15, anyChar)
      .map(x => {
        val x_string = x.mkString
        println(s"#total_length raw string is '$x_string'")
        val length = Integer.parseInt(x.mkString, 2)
        println(s"#total_length length is $length")
        LengthMode.BitLength.apply(length)
      })
      .debug("total_length_mode")
  val subpacket_count_mode =
    char('1') *> repeat(11, anyChar)
      .map(x => {
        val x_string = x.mkString
        println(s"#subpacket raw string is '$x_string'")
        val length = Integer.parseInt(x.mkString, 2)
        println(s"#subpacket length is $length")
        LengthMode.PacketLength.apply(length)
      })
      .debug("subpacket_count_mode")

  def past_position(position: Int): Parsley[Unit] =
    (for
      curr_position <- col
      _ <- lookAhead(satisfy(_ => curr_position + 1 >= position))
    yield ()).debug(s"past_position: $position")

  val literal_packet = lift2(
    Packet.Literal.apply,
    attempt(p_version.map(_.mkString) <* literal),
    value_chunks.map(_.mkString)
  )

  val op_packet = (for
    version <- p_version.map(_.mkString)
    command <- command.map(_.mkString)
    length <- total_length_mode <|> subpacket_count_mode
    subpacket_start_position <- col
    subpackets: Seq[Packet] <- length match {
      case LengthMode.PacketLength(n) =>
        repeat(n, packet).map(_.toList).debug(s"Looking for $n packets")
      case LengthMode.BitLength(n) =>
        someUntil(packet, past_position(subpacket_start_position + n))
          .debug(s"Looking for packets until $n bits")
    }
  yield Packet.Operator(version, command, subpackets)).debug("op_packet")

  def parse(input: String): Packet =
    packets.parse(input) match {
      case Success(p) => p
      case Failure(e) => throw new Exception(e.toString)
    }

  def toInt(input: String): Int = Integer.parseInt(input, 2)

  enum Packet(version: String):
    case Literal(version: String, contents: String) extends Packet(version)
    case Operator(version: String, op: String, contents: Seq[Packet])
        extends Packet(version)
    override def toString: String = this match {
      case Literal(version, contents) =>
        s"Literal(${toInt(version)}, ${toInt(contents)})"
      case Operator(version, op, contents) =>
        s"Operator(${toInt(version)}, ${toInt(op)}, ${contents.mkString(", ")})"
    }

  val hexToBinaryMap = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )
  def toBinary(input: String): String = {
    input.toList.map(hexToBinaryMap.apply).mkString
  }

  val input =
    """020D74FCE27E600A78020200DC298F1070401C8EF1F21A4D6394F9F48F4C1C00E3003500C74602F0080B1720298C400B7002540095003DC00F601B98806351003D004F66011148039450025C00B2007024717AFB5FBC11A7E73AF60F660094E5793A4E811C0123CECED79104ECED791380069D2522B96A53A81286B18263F75A300526246F60094A6651429ADB3B0068937BCF31A009ADB4C289C9C66526014CB33CB81CB3649B849911803B2EB1327F3CFC60094B01CBB4B80351E66E26B2DD0530070401C82D182080803D1C627C330004320C43789C40192D002F93566A9AFE5967372B378001F525DDDCF0C010A00D440010E84D10A2D0803D1761045C9EA9D9802FE00ACF1448844E9C30078723101912594FEE9C9A548D57A5B8B04012F6002092845284D3301A8951C8C008973D30046136001B705A79BD400B9ECCFD30E3004E62BD56B004E465D911C8CBB2258B06009D802C00087C628C71C4001088C113E27C6B10064C01E86F042181002131EE26C5D20043E34C798246009E80293F9E530052A4910A7E87240195CC7C6340129A967EF9352CFDF0802059210972C977094281007664E206CD57292201349AA4943554D91C9CCBADB80232C6927DE5E92D7A10463005A4657D4597002BC9AF51A24A54B7B33A73E2CE005CBFB3B4A30052801F69DB4B08F3B6961024AD4B43E6B319AA020020F15E4B46E40282CCDBF8CA56802600084C788CB088401A8911C20ECC436C2401CED0048325CC7A7F8CAA912AC72B7024007F24B1F789C0F9EC8810090D801AB8803D11E34C3B00043E27C6989B2C52A01348E24B53531291C4FF4884C9C2C10401B8C9D2D875A0072E6FB75E92AC205CA0154CE7398FB0053DAC3F43295519C9AE080250E657410600BC9EAD9CA56001BF3CEF07A5194C013E00542462332DA4295680"""
}
