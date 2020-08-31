### Fourth order tensors ###
       function inv_4(T) 
        implicit none
c
        type(Tensor4), intent(in) :: T
        type(Tensor4) ::  inv_4
        real(kind=8), dimension(36) :: ten_4_to_list_36
        real, dimension(6,6) :: T_M66, T_M66inv
        logical :: OK_FLAG
c
c Here we use the 6x6 matrix inversion
c by David G. Simpson available at [http://web.hku.hk/~gdli/UsefulFiles/matrix/m66inv_f90.txt]
c With the existing functions we transform the fourth order tensor 'T'
c into its 6x6 Voigt-Matrix representation
       ten_4_to_list_36 = ten_to_list( T )
       T_M66 = list_to_array( ten_4_to_list_36 )
c Compute the inverse of the 6x6 matrix
         !  call M66INV (T_M66, T_M66inv, OK_FLAG)
         !  if ( OK_FLAG == .False. )
         !     write(*,*) "Failed to compute the inverse of the 
         !&fourth order tensor"
         !     pause
         ! endif
       call matrixinv(T_M66, T_M66inv, 6)
c Transform the 6x6 matrix back to its 4th order tensor representation
      inv_4 = array_to_ten(T_M66inv)
c
c @todo A more efficient implementation might be achieved by following an
c algorithm similar to lines 1294ff in
c [https://dealii.43-1.org/documentation/9.0.0/doxygen/deal.II/symmetric__tensor_8h_source.html]
c
       end function inv_4