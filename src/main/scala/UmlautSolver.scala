object UmlautSolver extends App {
    import java.io._
    val pw = new PrintWriter(new File("umlaut.txt"))
    val bw = new BufferedWriter(new FileWriter(new File("umlautbw.txt")))
    pw.write("aouAOUäöüÄÖÜ")
    bw.write("aouAOUäöüÄÖÜ")
    pw.close()
    bw.close()
    println("printing done")
}