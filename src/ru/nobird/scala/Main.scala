package ru.nobird.scala

import java.io.FileWriter

import ru.nobird.scala.hw._

/**
  * Created by ruslandavletshin on 10/02/16.
  */
object Main {
    def main(args: Array[String]) {
        if (args.length < 2) {
            println("Первый аргумент - номер задания [1-5], следующие аргументы - имя файла для ввода и вывода.")
        } else {
            val reader = new Reader(args(1))
            var writer = new FileWriter(args(2))
            val time = System.currentTimeMillis()
            args(0) match {
                case "1" => HW1.execute(reader, writer)
                case "2" => HW2.execute(reader, writer)
                case "3" => HW3.execute(reader, writer)
                case "4" =>
                    val (ok, e_m) = HW4.execute(reader, writer)
                    if (!ok) {
                        writer.close()
                        writer = new FileWriter(args(2))
                        writer.write(e_m)
                    }
                case "5" => HW5.execute(reader, writer)
                case default => println("Неверный номер задания")
            }
            println(System.currentTimeMillis() - time + "ms")
            writer.close()
            reader.close()
        }
    }
}
