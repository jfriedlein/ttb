
      function stress_push_forward_2(T,F)
        implicit none
        
        type(Tensor2), intent(in) :: T,F
        type(Tensor2) :: stress_push_forward_2
        
          stress_push_forward_2 = 1./det(F) * F
     &                            * T * transpose(F)
     
      end function stress_push_forward_2

      function tangent_push_forward_4(T,F)
c From deal.II-9.1.1 file transformations.h line 921ff
c Verified against the deal.II function
c In constrast to the deal.II function, we multiply the result by 1/det(F) as in
c the NLFE-script (sorry not online).
c @todo Differs from the LS-Dyna push_forward_4s when comparing the results
c
        implicit none
        
        type(Tensor4), intent(in) :: T
        type(Tensor2), intent(in) :: F
        type(Tensor4) :: tangent_push_forward_4, tmp, tmp2, tmp3
        integer :: I_, j, K, L, JJ ! 'JJ' for capital J
c Init the variables to zero, cause we use the equivalent of '+='
        tmp%abcd = 0.d0
        tmp2%abcd = 0.d0
        tmp3%abcd = 0.d0
        tangent_push_forward_4%abcd = 0.d0
c
        ! @test
        ! {           
            !F%ab(1,1)=      1.00294501234123 ;
            !F%ab(2,1)=      -7.715037228814872E-004  ;
            !F%ab(3,1)=      2.393000059730825E-004;
            !F%ab(1,2)=      7.881376893604719E-004 ;
            !F%ab(2,2)=      0.999319192852103  ;
            !F%ab(3,2)=      -2.461142185442748E-004 ;
            !F%ab(1,3)=      -1.890082683008460E-004;
            !F%ab(2,3)=        1.775084041084850E-004 ;
            !F%ab(3,3)=      0.999003968062282  ;
            !
            !T = deviatoric_I4(Eye)
            !T = T + (symmetrize(F).dya.symmetrize(F))
         ! }
c
      ! Push-forward of the indices one-by-one
      do I_=1,3
         do j=1,3
             do K=1,3
                do L=1,3
                   do JJ=1,3
                     tmp%abcd(I_,j,K,L) = tmp%abcd(I_,j,K,L)
     &                             + F%ab(j,JJ) * T%abcd(I_,JJ,K,L)
                   enddo
                enddo
              enddo
          enddo
      enddo
c
      do I_=1,3
         do j=1,3
             do K=1,3
                do L=1,3
                   do JJ=1,3  
                       tmp2%abcd(I_,j,K,L) = tmp2%abcd(I_,j,K,L)
     &                             + tmp%abcd(I_,j,K,JJ) * F%ab(L,JJ)
                   enddo
                enddo
              enddo
          enddo
      enddo
c    
      do I_=1,3
         do j=1,3
             do K=1,3
                do L=1,3
                   do JJ=1,3       
                       tmp3%abcd(I_,j,K,L) = tmp3%abcd(I_,j,K,L)
     &                             + F%ab(I_,JJ) * tmp2%abcd(JJ,j,K,L)
                 enddo
                enddo
              enddo
          enddo
      enddo
c
      do I_=1,3
         do j=1,3
             do K=1,3
                do L=1,3
                   do JJ=1,3   
                      tangent_push_forward_4%abcd(I_,j,K,L) = 
     &                         tangent_push_forward_4%abcd(I_,j,K,L)
     &                             + F%ab(K,JJ) * tmp3%abcd(I_,j,JJ,L)
                 enddo
                enddo
              enddo
          enddo
      enddo
c
      tangent_push_forward_4 = 1./det(F) * tangent_push_forward_4
c
      end function tangent_push_forward_4
