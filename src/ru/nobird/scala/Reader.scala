package ru.nobird.scala

import java.io.{FileReader, BufferedReader}
import java.util.StringTokenizer

/**
  * Created by ruslandavletshin on 07/02/16.
  */
class Reader(file: String) extends BufferedReader(new FileReader(file)){
    var st:StringTokenizer = null

    def next():String = {
        if (st == null) st = new StringTokenizer(readLine())
        while (ready() && !st.hasMoreTokens) {
            st = new StringTokenizer(readLine())
        }

        st.nextElement().toString
    }

    def nextInt(): Int = next().toInt
}