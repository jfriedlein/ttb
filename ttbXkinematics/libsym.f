c Compute the symmetric part of a second order tensor
      function symmetrize_2(T)
        implicit none
        
        type(Tensor2), intent(in) :: T
        type(Tensor2) :: symmetrize_2
        
        symmetrize_2%ab = T%ab
     
        ! Symmetrize the off-diagonal elements
         symmetrize_2%ab(1,2) = 0.5 * ( T%ab(1,2) + T%ab(2,1) )
         symmetrize_2%ab(1,3) = 0.5 * ( T%ab(1,3) + T%ab(3,1) )
         symmetrize_2%ab(2,3) = 0.5 * ( T%ab(2,3) + T%ab(3,2) )

         symmetrize_2%ab(2,1) = symmetrize_2%ab(1,2)
         symmetrize_2%ab(3,1) = symmetrize_2%ab(1,3)
         symmetrize_2%ab(3,2) = symmetrize_2%ab(2,3)
        
        ! Leave the diagonal values unchanged
        
       end function symmetrize_2

