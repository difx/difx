#  A script to generated the GPS C/A code
#  For reference see
#  "Understanding GPS Principles and Applications", Elliott D. Kaplan
#  "Global Positioning System: Theory and Applications Volume 1", Parkinson, J., Enge, P.
#
#  Essentialy the C/A code is a unique sequence of 1023 bits (1s and 0s) that are generated
#  for each GPS satellite.  Two 10 bit registers are used to mix around bits to create
#  a psuedorandom sequence using XOR operations.  It is inefficient to describe the process
#  in words so please see a diagram with description in a standard GPS textbook or the internet
#
#  This routine will accept a GPS satellite PRN Number and will create its 1023 bit code
#  and return a 1023 element array of 1s and 0s
#
#  A list of the PRN number and its corresponding bits to XOR for the G2 register
#
#  1(2,6)   2(3,7)  3(4,8)  4(5,9)  5(1,9)  6(2,10) 7(1,8)
#  8(2,9)   9(3,10) 10(2,3) 11(3,4) 12(5,6) 13(6,7) 14(7,8)
#  15(8,9)  16(9,10) 17(1,4) 18(2,5) 19(3,6) 20(4,7)
#  21(5,8)  22(6,9) 23(1,3) 24(4,6) 25(5,7) 26(6,8) 27(7,9)
#  28(8,10) 29(1,6) 30(2,7) 31(3,8) 32(4,9)
#
#
#
#

def CA_code_gen(PRN):

    # Store the PRN number with its G2 XOR bits in a python dictionary
    # 2 dictionaries will be made for both bits remember register 1 is the 0 bit
    # so subtract 1 for indices
    b1 = {1:1, 2:2, 3:3, 4:4, 5:0, 6:1, 7:0, 8:1, 9:2, 10:1, 11:2, 12:4, 13:5, 14:6, 15:7, 16:8, 17:0, 18:1, 19:2, 20:3, 21:4, 22:5, 23:0, 24:3, 25:4, 26:5, 27:6, 28:7, 29:0, 30:1, 31:2, 32:3}
    b2 = {1:5, 2:6, 3:7, 4:8, 5:8, 6:9, 7:7, 8:8, 9:9, 10:2, 11:3, 12:5, 13:6, 14:7, 15:8, 16:9, 17:3, 18:4, 19:5, 20:6, 21:7, 22:8, 23:2, 24:5, 25:6, 26:7, 27:8, 28:9, 29:5, 30:6, 31:7, 32:8}
    
    code = []
    G1_out = []
    G2_out = []
    # Reset/initialize the 2 registers G1 and G2 to have all 1s
    G1 = [1,1,1,1,1,1,1,1,1,1]
    G2 = [1,1,1,1,1,1,1,1,1,1]
    
    # Generate the codes
    i = 1
    
    while i <= 1023:
        # The 10th bit is the output of G1, for G2 the output is the XOR of 2 bits
        # chosen by the PRN
        G1_out = G1[9]
        G2_out = (G2[ b1[PRN] ])^(G2[ b2[PRN] ])
        
        # The output code is then the XOR of these two
        code.append(G1_out^G2_out)

        # Next shift the registers, but save the XOR of the 3rd and 10th bits first
        # since this will become bit 1 for G1 register
        G1_temp = (G1[2])^(G1[9])
        G1[1:10] = G1[0:9]
        G1[0] = G1_temp
        
        # The G2 register is a little more complicated
        # bit 1 becomes the XOR of bits 2,3,6,8,9,10 
        # and then shift the bits    
        G2_temp = (G2[1])^(G2[2])^(G2[5])^(G2[7])^(G2[8])^(G2[9])
        G2[1:10] = G2[0:9]
        G2[0] = G2_temp
 
        i += 1

    # end while
    
    return code    

def main():
    # test of the 32 SV, the first 10 bits are only tested 
    SV1 = [1,1,0,0,1,0,0,0,0,0]
    SV2 = [1,1,1,0,0,1,0,0,0,0]
    SV3 = [1,1,1,1,0,0,1,0,0,0]
    SV4 = [1,1,1,1,1,0,0,1,0,0]
    SV5 = [1,0,0,1,0,1,1,0,1,1]
    SV6 = [1,1,0,0,1,0,1,1,0,1]
    SV7 = [1,0,0,1,0,1,1,0,0,1]
    SV8 = [1,1,0,0,1,0,1,1,0,0]
    SV9 = [1,1,1,0,0,1,0,1,1,0]
    SV10 = [1,1,0,1,0,0,0,1,0,0]
    SV11 = [1,1,1,0,1,0,0,0,1,0]
    SV12 = [1,1,1,1,1,0,1,0,0,0]
    SV13 = [1,1,1,1,1,1,0,1,0,0]
    SV14 = [1,1,1,1,1,1,1,0,1,0]
    SV15 = [1,1,1,1,1,1,1,1,0,1]
    SV16 = [1,1,1,1,1,1,1,1,1,0]
    SV17 = [1,0,0,1,1,0,1,1,1,0]
    SV18 = [1,1,0,0,1,1,0,1,1,1]
    SV19 = [1,1,1,0,0,1,1,0,1,1]
    SV20 = [1,1,1,1,0,0,1,1,0,1]
    SV21 = [1,1,1,1,1,0,0,1,1,0]
    SV22 = [1,1,1,1,1,1,0,0,1,1]
    SV23 = [1,0,0,0,1,1,0,0,1,1]
    SV24 = [1,1,1,1,0,0,0,1,1,0]
    SV25 = [1,1,1,1,1,0,0,0,1,1]
    SV26 = [1,1,1,1,1,1,0,0,0,1]
    SV27 = [1,1,1,1,1,1,1,0,0,0]
    SV28 = [1,1,1,1,1,1,1,1,0,0]
    SV29 = [1,0,0,1,0,1,0,1,1,1]
    SV30 = [1,1,0,0,1,0,1,0,1,1]
    SV31 = [1,1,1,0,0,1,0,1,0,1]
    SV32 = [1,1,1,1,0,0,1,0,1,0]

    error = 0
    test = CA_code_gen(1)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV1 != test[0:10]:
	error += 1
    test = CA_code_gen(2)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV2 != test[0:10]:
	error += 1
    test = CA_code_gen(3)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV3 != test[0:10]:
	error += 1
    test = CA_code_gen(4)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV4 != test[0:10]:
	error += 1    
    test = CA_code_gen(5)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV5 != test[0:10]:
	error += 1
    test = CA_code_gen(6)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV6 != test[0:10]:
	error += 1
    test = CA_code_gen(7)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV7 != test[0:10]:
	error += 1
    test = CA_code_gen(8)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV8 != test[0:10]:
	error += 1
    test = CA_code_gen(9)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV9 != test[0:10]:
	error += 1
    test = CA_code_gen(10)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV10 != test[0:10]:
	error += 1    
    test = CA_code_gen(11)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV11 != test[0:10]:
	error += 1
    test = CA_code_gen(12)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV12 != test[0:10]:
	error += 1
    test = CA_code_gen(13)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV13 != test[0:10]:
	error += 1
    test = CA_code_gen(14)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV14 != test[0:10]:
	error += 1
    test = CA_code_gen(15)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV15 != test[0:10]:
	error += 1
    test = CA_code_gen(16)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV16 != test[0:10]:
	error += 1    
    test = CA_code_gen(17)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV17 != test[0:10]:
	error += 1
    test = CA_code_gen(18)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV18 != test[0:10]:
	error += 1
    test = CA_code_gen(19)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV19 != test[0:10]:
	error += 1
    test = CA_code_gen(20)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV20 != test[0:10]:
	error += 1
    test = CA_code_gen(21)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV21 != test[0:10]:
	error += 1
    test = CA_code_gen(22)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV22 != test[0:10]:
	error += 1    
    test = CA_code_gen(23)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV23 != test[0:10]:
	error += 1
    test = CA_code_gen(24)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV24 != test[0:10]:
	error += 1
    test = CA_code_gen(25)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV25 != test[0:10]:
	error += 1
    test = CA_code_gen(26)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV26 != test[0:10]:
	error += 1    
    test = CA_code_gen(27)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV27 != test[0:10]:
	error += 1
    test = CA_code_gen(28)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV28 != test[0:10]:
	error += 1
    test = CA_code_gen(29)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV29 != test[0:10]:
	error += 1
    test = CA_code_gen(30)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV30 != test[0:10]:
	error += 1
    test = CA_code_gen(31)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV31 != test[0:10]:
	error += 1
    test = CA_code_gen(32)    # Print the first 10 chips
    length = len(test)
    if length != 1023:
	error += 1
    if SV32 != test[0:10]:
	error += 1    

    print "# of Errors are " + str(error)

if __name__=='__main__':
    main()    
