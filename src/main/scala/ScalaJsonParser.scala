/**
 * Created by yshnb on 2015/05/09.
 */

import scala.util.parsing.combinator._

object ScalaJsonParser {
  def main(args: Array[String]): Unit = {

    println(JsonParser("{'hoge':{'hoge':'hoge','hage':'hage'}}"))
  }
}

object JsonParser extends RegexParsers {
  override def skipWhitespace = false

  /*
   JSON-text = ws _value ws

   begin-array     = ws %x5B ws  ; [ left square bracket

   begin-object    = ws %x7B ws  ; { left curly bracket

   end-array       = ws %x5D ws  ; ] right square bracket

   end-object      = ws %x7D ws  ; } right curly bracket

   name-separator  = ws %x3A ws  ; : colon

   _value-separator = ws %x2C ws  ; , comma

   ws = *(
           %x20 /              ; Space
           %x09 /              ; Horizontal tab
           %x0A /              ; Line feed or New line
           %x0D )              ; Carriage return
   */
  def json_text = ws ~ _value ~ ws

  def begin_array = "["

  def begin_object = "{"

  def end_array = "]"

  def end_object = "}"

  def name_separator = ":"

  def value_separator = ","

  def ws = opt("""[\u0020\u0009\u000a\u000d]""".r)

  /*
   _value = false / null / true / object / array / number / string

   false = %x66.61.6c.73.65   ; false

   null  = %x6e.75.6c.6c      ; null

   true  = %x74.72.75.65      ; true
   */
  def _value: JsonParser.Parser[Any] = _false | _null | _true | _object | _array | number | string

  def _false = "false"

  def _null = "null"

  def _true = "true"

  /*
   object = begin-object [ member *( value-separator member ) ]
            end-object

   member = string name-separator value
   */

  def _object = begin_object ~ repsep(member, value_separator) ~ end_object

  def member = string ~ name_separator ~ _value

  /*
   array = begin-array [ value *( value-separator value ) ] end-array
   */
  def _array = begin_array ~ repsep(_value, value_separator) ~ end_array

  /*
   number = [ minus ] int [ frac ] [ exp ]

   decimal-point = %x2E       ; .

   digit1-9 = %x31-39         ; 1-9

   e = %x65 / %x45            ; e E

   exp = e [ minus / plus ] 1*DIGIT

   frac = decimal-point 1*DIGIT

   int = zero / ( digit1-9 *DIGIT )

   minus = %x2D               ; -

   plus = %x2B                ; +

   zero = %x30                ; 0
   */

  def number = opt(minus) ~ int ~ opt(frac) ~ opt(exp)

  def dicimal_point = "."

  def digit1_9 = """[\u0031-\u0039]""".r

  def e = "e" | "E"

  def exp = e ~ opt(minus | plus) ~ rep1(digit1_9)

  def frac = dicimal_point ~ rep1(digit1_9)

  def int = zero | rep1(digit1_9)

  def minus = "-"

  def plus = "+"

  def zero = "0"

  /*
   Value.

   string = quotation-mark *char quotation-mark

   char = unescaped /
       escape (
           %x22 /          ; "    quotation mark  U+0022
           %x5C /          ; \    reverse solidus U+005C
           %x2F /          ; /    solidus         U+002F
           %x62 /          ; b    backspace       U+0008
           %x66 /          ; f    form feed       U+000C
           %x6E /          ; n    line feed       U+000A
           %x72 /          ; r    carriage return U+000D
           %x74 /          ; t    tab             U+0009
           %x75 4HEXDIG )  ; uXXXX                U+XXXX

   escape = %x5C              ; \

   quotation-mark = %x22      ; "

   unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
   */
  def string = quotation_mark ~ rep1(char) ~ quotation_mark

  def char = unescaped | (escape ~ """[\u0022\u005c\u002f\u0062\u0066\u006e\u0072\u0074\u0075]""".r)

  def escape = "¥"

  def quotation_mark = "'"

//  def unescaped = """[\u0020-\u0021\u0023-\u005b\u005d-\u00ff]""".r
  def unescaped = """[a-zA-Z0-9]""".r

  def apply(input: String): Either[String, Any] = parseAll(json_text, input) match {
    case Success(json_text, next) => Right(json_text)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}

