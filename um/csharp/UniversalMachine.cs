using System;
using System.IO;
using System.Collections.Generic;
using System.Text;

namespace UniversalMachine
{
	class UM
	{
		private List<uint[]> allocatedMem = new List<uint[]> ();
		private List<int> freed = new List<int> ();

		// Registers.
		private uint[] regs = new uint[8];

		// Program counter.
		private uint pc = 0;

		// Input and output streams.
		private Stream input;
		private Stream output;

		public UM (string programPath) : this(programPath, Console.OpenStandardInput (), Console.OpenStandardOutput ())
		{
		}

		public UM (string programPath, Stream input, Stream output)
		{
			List<uint> program = new List<uint> ();
			BinaryReader reader = new BinaryReader (File.OpenRead (programPath));
			
			uint current;
			try 
			{
				while (true) 
				{
					byte b1 = reader.ReadByte ();
					byte b2 = reader.ReadByte ();
					byte b3 = reader.ReadByte ();
					byte b4 = reader.ReadByte ();
					current = (uint)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
					program.Add (current);
				}
			} 
			catch (EndOfStreamException)
			{
			}
			
			// Add the program as the first item - index 0.
			allocatedMem.Add (program.ToArray ());
			
			this.input = input;
			this.output = output;
		}

		// This is the instruction format.
		//
		//                              A     C
		//                              |     |
		//                              vvv   vvv                    
		//      .--------------------------------.
		//      |VUTSRQPONMLKJIHGFEDCBA9876543210|
		//      `--------------------------------'
		//       ^^^^                      ^^^
		//       |                         |
		//       operator number           B
		//
		//      Figure 2. Standard Operators

		public void Run ()
		{
			bool halt = false;
			while (!halt) {
				uint instr = allocatedMem[0][pc];
				
				// Extract the register "indexes" and the instruction.
				uint a = (instr >> 6) & 7;
				uint b = (instr >> 3) & 7;
				uint c = instr & 7;
				uint opCode = instr >> 28;
				
				switch (opCode) {
				case 0:
					// Conditional move.
					// The register A receives the value in register B,
					// unless the register C contains 0.
					if (regs[c] != 0) {
						regs[a] = regs[b];
					}
					break;
				case 1:
					// Array Index.
					// The register A receives the value stored at offset
					// in register C in the array identified by B.
					regs[a] = allocatedMem[(int)regs[b]][regs[c]];
					break;
				case 2:
					// Array Amendment.
					// The array identified by A is amended at the offset
					// in register B to store the value in register C.
					allocatedMem[(int)regs[a]][regs[b]] = regs[c];
					break;
				case 3:
					// Addition.
					// The register A receives the value in register B plus 
					// the value in register C, modulo 2^32.
					regs[a] = unchecked(regs[b] + regs[c]);
					break;
				case 4:
					// Multiplication.
					// The register A receives the value in register B times
					// the value in register C, modulo 2^32.
					regs[a] = unchecked(regs[b] * regs[c]);
					break;
				case 5:
					// Division.
					// The register A receives the value in register B
					// divided by the value in register C, if any, where
					// each quantity is treated treated as an unsigned 32
					// bit number.
					regs[a] = regs[b] / regs[c];
					break;
				case 6:
					// Not-And.
					// Each bit in the register A receives the 1 bit if
					// either register B or register C has a 0 bit in that
					// position.  Otherwise the bit in register A receives
					// the 0 bit.
					regs[a] = ~(regs[b] & regs[c]);
					break;
				case 7:
					halt = true;
					break;
				case 8:
					// Allocation.
					// A new array is created with a capacity of platters
					// commensurate to the value in the register C. This
					// new array is initialized entirely with platters
					// holding the value 0. A bit pattern not consisting of
					// exclusively the 0 bit, and that identifies no other
					// active allocated array, is placed in the B register.
					uint[] newArr = new uint[regs[c]];
					// See if we have any free memory positions to reuse.
					if (freed.Count > 0) {
						// We take the index at the end of the list
						// because removing that item is much faster.
						int index = freed[freed.Count - 1];
						allocatedMem[index] = newArr;
						freed.RemoveAt (freed.Count - 1);
						regs[b] = (uint)index;
					} else {
						allocatedMem.Add (newArr);
						regs[b] = (uint)allocatedMem.Count - 1;
					}
					break;
				case 9:
					// Abandonment.
					// The array identified by the register C is abandoned.
					// Future allocations may then reuse that identifier.
					freed.Add ((int)regs[c]);
					break;
				case 10:
					// Output.
					// The value in the register C is displayed on the console
					// immediately. Only values between and including 0 and 255
					// are allowed.
					output.WriteByte ((byte)regs[c]);
					break;
				case 11:
					// Input.
					// The universal machine waits for input on the console.
					// When input arrives, the register C is loaded with the
					// input, which must be between and including 0 and 255.
					// If the end of input has been signaled, then the 
					// register C is endowed with a uniform value pattern
					// where every place is pregnant with the 1 bit.
					int i = input.ReadByte ();
					if (i == -1) {
						regs[c] = 0xFFFFFFFF;
					} else {
						regs[c] = (uint)i;
					}
					break;
				case 12:
					// Load Program.
					
					// The array identified by the B register is duplicated
					// and the duplicate shall replace the '0' array,
					// regardless of size. The execution finger is placed
					// to indicate the platter of this array that is
					// described by the offset given in C, where the value
					// 0 denotes the first platter, 1 the second, et
					// cetera.
					//
					// The '0' array shall be the most sublime choice for
					// loading, and shall be handled with the utmost
					// velocity.
					
					// If the array is the 0 array, we're just dealing with a jmp.
					if (regs[b] != 0) {
						uint[] arr = allocatedMem[(int)regs[b]];
						uint[] dup = new uint[arr.Length];
						arr.CopyTo (dup, 0);
						allocatedMem[0] = dup;
					}
					
					pc = regs[c] - 1;
					break;
				case 13:
					// Orthography.
					// The value indicated is loaded into the register A
					// forthwith.
					
					// Register A is in another place here.
					a = (instr >> 25) & 7;
					uint val = instr & 0x1FFFFFF;
					regs[a] = val;
					break;
					
				}
				
				pc++;
			}
			
		}
	}

	class MainClass
	{
		// On unix the following is an OK way of extracting UMIX.
		// echo -e "(\\\b.bb)(\\\v.vv)06FHPVboundvarHRAk\np" | mono UniversalMachine.exe codex.umz > umix
		// Then, with an editor, keep only the text that follows "UM program follows colon:".
		public static void Main (string[] args)
		{
			if (args.Length == 1) {
				UM um = new UM (args[0]);
				um.Run ();
			} else {
				//Console.WriteLine("Specify the file to load as an argument.");
				UM um = new UM ("sandmark.umz");
				um.Run ();
			}
		}
	}
}
