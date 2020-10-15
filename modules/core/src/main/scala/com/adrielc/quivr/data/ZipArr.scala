package com.adrielc.quivr.data

trait ZipArr[F[_, _], A, D] {
  type B
  type C

  def back: Option[ZipArr[F, A, B]]

  def focus: F[B, C]

  def forward: Option[ZipArr[F, A, B]]
}
