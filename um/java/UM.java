import java.io.*;
import java.util.ArrayList;


public class UM {

	private ArrayList<int[]> allocatedMem = new ArrayList<int[]>();
	private ArrayList<Integer> freed = new ArrayList<Integer>();
	private int[] regs = new int[8];
	
	private int pc;
	
	// Input and output streams.
	private InputStream input;
	private OutputStream output;
	
	public UM(String programPath) throws IOException {
		this(programPath, System.in, System.out);
	}
	
	public UM(String programPath, InputStream input, OutputStream output)
			throws IOException {
		// Find the size of the program file.
		File f = new File(programPath);
		if (f.length() % 4 != 0) {
			throw new IOException("File is corrupt - the number of bytes is not a multiple of 4");
		}
		
		FileInputStream in = new FileInputStream(programPath);
		
		int[] program = new int[(int)f.length() / 4];
		byte[] bytes = new byte[4];
		int instr;
		for (int i = 0; i < program.length; i++) {
			in.read(bytes);
			// & with 0xFF gives us the unsigned byte value.
			instr = ((bytes[0] & 0xFF) << 24) | ((bytes[1] & 0xFF) << 16) | ((bytes[2] & 0xFF) << 8) | (bytes[3] & 0xFF);
			program[i] = instr;
		}
		allocatedMem.add(program);
		this.input = input;
		this.output = output;
	}
	
	public void run() throws IOException {
		boolean halt = false;
		while(!halt) {
			int instr = allocatedMem.get(0)[pc];
			int a = (instr >> 6) & 7;
			int b = (instr >> 3) & 7;
			int c = instr & 7;
			int opCode = instr >>> 28; // Note unsigned shift!
			
			switch(opCode) {
			case 0:
				// Conditional move.
				if (regs[c] != 0) {
					regs[a] = regs[b];
				}
				break;
			case 1:
				// Array index.
				regs[a] = allocatedMem.get(regs[b])[regs[c]];
				break;
			case 2:
				// Array amendment.
				allocatedMem.get(regs[a])[regs[b]] = regs[c];
				break;
			case 3:
				// Addition. 
				// Signed versus unsigned doesn't matter.
				regs[a] = regs[b] + regs[c];
				break;
			case 4:
				// Multiplication.
				// Use the unsigned value as a long.
				regs[a] = (int) ((regs[b] & 0xFFFFFFFFL) * (regs[c] & 0xFFFFFFFFL));
				break;
			case 5:
				// Division.
				// Use the unsigned value as a long.
				regs[a] = (int) ((regs[b] & 0xFFFFFFFFL) / (regs[c] & 0xFFFFFFFFL));
				break;
			case 6:
				// Not-and.
				regs[a] = ~(regs[b] & regs[c]);
				break;
			case 7:
				// Halt.
				halt = true;
				break;
			case 8:
				// Allocation.
				int[] newArr = new int[regs[c]];
				
				// See if we have any free memory.
				if (freed.size() > 0) {
					// We take the index at the end of the list
					// because removing that item is much faster.
					int index = freed.get(freed.size() - 1);
					allocatedMem.set(index, newArr);
					freed.remove(freed.size() - 1);
					regs[b] = index;
				} else {
					allocatedMem.add(newArr);
					regs[b] = allocatedMem.size() - 1;
				}
				break;
			case 9:
				// Abandonment.
				freed.add(regs[c]);
				break;
			case 10:
				// Output.
				output.write(regs[c]);
				output.flush();
				break;
			case 11:
				// Input.
				int in = input.read();
				regs[c] = in;
				break;
			case 12:
				// Load program.
				// Only if we're not dealing with the 0 array do we need to
				// copy.
				if (regs[b] != 0) {
					int[] arr = allocatedMem.get(regs[b]);
					int[] dup = arr.clone();
					allocatedMem.set(0, dup);
				}
				pc = regs[c] - 1;
				break;
			case 13:
				// Orthography.
				a = (instr >> 25) & 7;
				regs[a] = instr & 0x1FFFFFF;
				break;
			default:
				System.out.println("Unknown op code encountered.");
				halt = true;
			}
			
			pc++;
		}
	}
	
	public static void main(String[] args) {
		try {
			UM um = new UM(args[0]);
			um.run();
		} catch (IOException e) {
			System.out.println(e);
		}
	}
}

