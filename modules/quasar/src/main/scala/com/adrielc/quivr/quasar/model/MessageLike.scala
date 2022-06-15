package com.adrielc.quivr.quasar
package model

import java.time.ZonedDateTime

import io.circe.{Decoder, Json}
import spire.math.ULong

sealed trait OutboundMessageLike {

  val content: String

}

case class OutboundMessage(content: String) extends OutboundMessageLike

sealed trait MessageLike extends OutboundMessageLike {

  val id: ULong
  val channelId: ULong
  val author: Either[/* Webhook */ Json, /* UserLike */ Json]
  val content: String
  val timestamp: ZonedDateTime
  val editedTimestamp: Option[ZonedDateTime]
  val tts: Boolean
  val mentions: List[/* UserLike */ Json]
  val mentionRoles: List[ULong]
  val attachments: List[/* Attachment */ Json]
  val embeds: List[/* Embed */ Json]
  val pinned: Boolean
  val messageType: MessageType

}

object MessageLike {

  implicit val decodeMessageLike: Decoder[MessageLike] = GuildMessage.decodeGuildMessage.map(identity) // TODO

}

case class GuildMessage(
  id: ULong,
  channelId: ULong,
  guildId: ULong,
  author: Either[/* Webhook */ Json, /* Member */ Json],
  content: String,
  timestamp: ZonedDateTime,
  editedTimestamp: Option[ZonedDateTime],
  tts: Boolean,
  mentions: List[/* Member */ Json],
  mentionRoles: List[ULong],
  attachments: List[/* Attachment */ Json],
  embeds: List[/* Embed */ Json],
  pinned: Boolean,
  messageType: MessageType
) extends MessageLike

object GuildMessage {

  implicit val decodeGuildMessage: Decoder[GuildMessage] = Decoder.instance { cursor =>
    for {
      id <- cursor.downField("id").as[ULong]
      channelId <- cursor.downField("channel_id").as[ULong]
      guildId <- cursor.getOrElse("guild_id")(ULong.MinValue)
      author <- /* TODO */ cursor.downField("author").as[Json]
      content <- cursor.downField("content").as[String]
      timestamp <- cursor.downField("timestamp").as[ZonedDateTime]
      editedTimestamp <- cursor.downField("edited_timestamp").as[Option[ZonedDateTime]]
      tts <- cursor.downField("tts").as[Boolean]
      mentions <- cursor.downField("mentions").as[List[/* Member */ Json]]
      mentionRoles <- cursor.downField("mention_roles").as[List[ULong]]
      attachments <- cursor.downField("attachments").as[List[/* Attachment */ Json]]
      embeds <- cursor.downField("embeds").as[List[/* Embed */ Json]]
      pinned <- cursor.downField("pinned").as[Boolean]
      messageType <- Right(MessageType.Default) // TODO cursor.downField("type").as[MessageType]
    } yield GuildMessage(
      id,
      channelId,
      guildId,
      Right(author),
      content,
      timestamp,
      editedTimestamp,
      tts,
      mentions,
      mentionRoles,
      attachments,
      embeds,
      pinned,
      messageType
    )
  }

}

sealed trait MessageType
object MessageType {
  case object Default extends MessageType
}
