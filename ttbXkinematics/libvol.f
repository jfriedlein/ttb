c Compute the volumetric part of second order tensor
c @todo Maybe use this vol() for the computation of the deviatoric part
      function vol_2(T)
        implicit none
        
        type(Tensor2), intent(in) :: T
        type(Tensor2) :: vol_2,Eye
        
        Eye = identity2(Eye)
        vol_2%ab = tr(T)/3.d0*Eye%ab
     
       end function vol_2
       
       function vol_2s(T)
        implicit none
        
        type(Tensor2s), intent(in) :: T
        type(Tensor2s) :: vol_2s,Eye
        
        Eye = identity2(Eye)
        vol_2s%a6 = tr(T)/3.d0*Eye%a6
     
      end function vol_2s
