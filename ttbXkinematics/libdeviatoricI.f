      function dev_I4(T)
        implicit none
        
        type(Tensor2), intent(in) :: T
        type(Tensor2) :: Eye
        type(Tensor4) :: dev_I4
        integer :: i,j,k,l
        
        dev_I4%abcd = 0.d0
        Eye = identity2(T)
        forall(i=1:3,j=1:3,k=1:3,l=1:3)
     *    dev_I4%abcd(i,j,k,l) = 
     *                - 1.d0/3.d0*Eye%ab(i,j)*Eye%ab(k,l)
     *                + 1.d0/2.d0 * Eye%ab(i,k)*Eye%ab(j,l)
     *                + 1.d0/2.d0 * Eye%ab(i,l)*Eye%ab(j,k)
     
       end function dev_I4
       
c @todo add function dev_I4s(T)

