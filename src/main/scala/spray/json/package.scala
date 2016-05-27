/*
 * Copyright (C) 2009-2011 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray

import scala.json.ast._
import collection.immutable

package object json {

  type JsValue = JValue
  type JsString = JString
  val JsString = JString
  val JsNull = JNull
  type JsNumber = JNumber
  val JsNumber = JNumber
  type JsArray = JArray
  val JsArray = JArray
  type JsObject = JObject
  val JsObject = JObject
  type JsBoolean = JBoolean
  val JsBoolean = JBoolean
  val JsTrue = JTrue
  val JsFalse = JFalse

  type JsField = (String, JsValue)

  def deserializationError(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) = throw new DeserializationException(msg, cause, fieldNames)
  def serializationError(msg: String) = throw new SerializationException(msg)

  def jsonReader[T](implicit reader: JsonReader[T]) = reader
  def jsonWriter[T](implicit writer: JsonWriter[T]) = writer 
  
  implicit def pimpAny[T](any: T) = new PimpedAny(any)
  implicit def pimpString(string: String) = new PimpedString(string)

  implicit class jValueHelper(jsValue: JsValue) {
    override def toString = compactPrint
    def toString(printer: (JsValue => String)) = printer(jsValue)
    def compactPrint = CompactPrinter(jsValue)
    def prettyPrint = PrettyPrinter(jsValue)
    def convertTo[T :JsonReader]: T = jsonReader[T].read(jsValue)

    def asJsObject(errorMsg: String = "JSON object expected"): JsObject = deserializationError(errorMsg)

    def asJsObject: JsObject = asJsObject()
  }

  implicit class jObjectHelper(jsObject: JsObject) {
    def asJsObject(errorMsg: String) = jsObject
    def getFields(fieldNames: String*): immutable.Seq[JsValue] = fieldNames.flatMap(jsObject.value.get)(collection.breakOut)
  }

  implicit class jObjectObjectHelper(jsObject: JsObject.type) {
    val empty = JsObject()
  }

  implicit class jNumberObjectHelper(jsNumber: JsNumber.type) {
    val zero: JsNumber = JsNumber(0)
  }

  implicit class jStringObjectHelper(jString: JsString.type) {
    val empty = JsString("")
  }

  implicit class jArrayObjectHelper(jsArray: JsArray.type) {
    val empty = JsArray()
  }

}

package json {

  case class DeserializationException(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)

  private[json] class PimpedAny[T](any: T) {
    def toJson(implicit writer: JsonWriter[T]): JsValue = writer.write(any)
  }

  private[json] class PimpedString(string: String) {
    @deprecated("deprecated in favor of parseJson", "1.2.6")
    def asJson: JsValue = parseJson
    def parseJson: JsValue = JsonParser(string)
  }
}
