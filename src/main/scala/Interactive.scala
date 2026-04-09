object Interactive {

  // Function to display a menu with options in terminal
  def showMenu(title: String, options: List[String]): Unit = {
    println(Formatters.formatTitle(title))

    println("0. Volver")

    options.zipWithIndex.foreach {
      case (option, i) =>
        println(s"${i + 1}. $option")
    }
  }

  // Function to ask user for a number input and validate it
  def askOption(max: Int): Int = {
    def loop(): Int = {
      print("\nSeleccione un item: ")

      scala.io.StdIn.readLine() match {
        case input if input.forall(_.isDigit) =>
          val value = input.toInt

          if (value >= 0 && value <= max) value
          else {
            println("Valor fuera de rango.")
            loop()
          }

        case _ =>
          println("Entrada inválida.")
          loop()
      }
    }

    loop()
  }
}