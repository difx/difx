C Inputs.i
C
C   Include file for external inputs, etc.  98.03.05, DG
        Character*80 External_inputs, Ex_sites, Ex_stars, Ex_ocean,
     *               Ex_EOP
        Logical*4    External_aprioris, Input_sites, Input_stars,
     *               Input_ocean, Input_EOP
        Common /Extrnl/External_inputs, Ex_sites, Ex_stars, Ex_ocean, 
     *               Ex_EOP, External_aprioris, Input_sites, 
     *               Input_stars, Input_ocean, Input_EOP
C
