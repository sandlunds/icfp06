import java.io.{InputStream, FileInputStream, IOException, File}
import java.util.ArrayList

/**
 * Created by IntelliJ IDEA.
 * User: simon
 * Date: 2012-01-30
 * Time: 20:11
 * Implementation of the Universal Machine.
 * Pretty much a direct translation of my Java version of this program.
 */

object UniversalMachine {
  private val allocatedMem = new ArrayList[Array[Int]]
  private val freed = new ArrayList[Int]
  private val regs = new Array[Int](8)

  private var pc: Int = 0

  def openFile(programPath: String): (InputStream, Long) = {
    val file = new File(programPath)
    if (file.length() % 4 != 0) {
      throw new IOException("File is corrupt - the number of bytes is not a multiple of 4")
    }
    (new FileInputStream(file), file.length())
  }

  def readFile(programPath: String) {
    val (inputStream, length) = openFile(programPath)
    // Read the program contained in inputStream as a sequence of (length/4) 32-bit words.
    val program = new Array[Int]((length / 4).intValue)
    val bytes = new Array[Byte](4)
    var pos = 0;
    while (inputStream.read(bytes) != -1) {
      program(pos) = ((bytes(0) & 0xFF) << 24) | ((bytes(1) & 0xFF) << 16) | ((bytes(2) & 0xFF) << 8) | (bytes(3) & 0xFF)
      pos += 1
    }
    allocatedMem.add(program)
  }
  
  def run() {
    var halt = false;
    while(!halt) {
      val instr = allocatedMem.get(0)(pc)
      val (a, b, c) = ((instr >> 6) & 7, (instr >> 3) & 7, instr & 7)
      val opCode = instr >>> 28;

      opCode match {
        case 0 => if (regs(c) != 0){ regs(a) = regs(b) }
        case 1 => regs(a) = allocatedMem.get(regs(b))(regs(c))
        case 2 => allocatedMem.get(regs(a))(regs(b)) = regs(c)
        case 3 => regs(a) = regs(b) + regs(c)
        // We want to treat the Ints as unsigned.
        case 4 => regs(a) = ((regs(b) & 0xFFFFFFFFL) * (regs(c) & 0xFFFFFFFFL)).intValue
        case 5 => regs(a) = ((regs(b) & 0xFFFFFFFFL) /  (regs(c) & 0xFFFFFFFFL)).intValue
        case 6 => regs(a) = ~(regs(b) & regs(c));
        case 7 => halt = true
        case 8 => {
          // Allocate new memory.
          val allocated = new Array[Int](regs(c))
          if (freed.size() > 0) {
            val index = freed.get(freed.size() - 1)
            allocatedMem.set(index, allocated)
            freed.remove(freed.size() - 1)
            regs(b) = index;
          } else {
            allocatedMem.add(allocated)
            regs(b) = allocatedMem.size() - 1
          }
        }
        case 9 => freed.add(regs(c))
        case 10 => printf("%c", regs(c))
        case 11 => regs(c) = readByte()
        case 12 => {
          // Load program.
          // Only if we're not dealing with the 0 array do we need to copy.
          if (regs(b) != 0) {
            val dup = allocatedMem.get(regs(b)).clone()
            allocatedMem.set(0, dup)
          }
          pc = regs(c) - 1 // We add 1 before the end of the loop.
        }
        case 13 => {
          // Orthography.
          val a = (instr >> 25) & 7;
          regs(a) = instr & 0x1FFFFFF;
        }
        case _ => {
          println("Unknown opcode encountered: " + opCode)
          halt = true;
        }
      }

      pc += 1
    }
  }

  def main(args: Array[String]) {
    readFile("sandmark.umz")
    run()
  }
}