

      integer*2 function getunit( )

      integer*4 next_unit
      common /my_getunit/ next_unit

      data next_unit /20/

      getunit = next_unit
      next_unit = next_unit + 1

      end

