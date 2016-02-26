package ru.nobird.scala.hw

import java.io.FileWriter

import ru.nobird.scala.Reader

/**
  * Created by ruslandavletshin on 10/02/16.
  */
abstract class HW {
    def execute(reader: Reader, writer: FileWriter): (Boolean, String)
}
