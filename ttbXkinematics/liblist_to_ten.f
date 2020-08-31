!c Retrieve a fourth order tensor from a list with 36 entries       
!       function list_36_to_ten_4(list_36)
!        !implicit none
!        
!       ! @todo maybe check size of list_36_to_ten_4
!        type(Tensor4), intent(in) :: list_36_to_ten_4
!        real(kind=8), dimension(36), intent(in) :: from
!        real, dimension(6,6) :: es
!        integer :: i,j
!        es(1:6,1:6) = asarray(asvoigt(T),6,6)
!
!        forall(i=1:6,j=1:6) ten_4_to_list_36(j+(i-1)*6) = es(i,j)
!
!      end function list_36_to_ten_4
!
!c Retrieve from a list with 36 entries the 6x6 matrix     
!      function array_6x6_to_list_36 (array_6x6)
!        !implicit none
!        
!        real(kind=8), dimension(36) :: array_6x6_to_list_36, list
!        real, dimension(6,6), intent(in) :: array_6x6
!        integer :: i,j
!
!        forall(i=1:6,j=1:6) list(j+(i-1)*6) = array_6x6(i,j)
!        
!      array_6x6_to_list_36 = list
!      
!      end function array_6x6_to_list_36
      
c Retrieve a fourth order tensor from a 6x6 matrix    
       function array_6x6_to_ten_4(array_6x6)
        !implicit none
        
        type(Tensor4):: array_6x6_to_ten_4, ten4
        real, dimension(6,6), intent(in) :: array_6x6
        integer :: i,j

      ! The following is based on the symstore_4 function (just vice versa) with additionally the symmetric entries
      ten4%abcd = 0.d0
        do i=1,3
         do j=1,3
           ten4%abcd(i,i,j,j) = array_6x6(i,j)
         enddo
        enddo
        
        ten4%abcd(1,2,1,2) = 0.25 * array_6x6(4,4)
        ten4%abcd(1,2,2,1) = ten4%abcd(1,2,1,2)
        ten4%abcd(2,1,1,2) = ten4%abcd(1,2,1,2)
        ten4%abcd(2,1,2,1) = ten4%abcd(1,2,1,2)

        ten4%abcd(2,3,2,3) = 0.25 * array_6x6(5,5)
        ten4%abcd(2,3,3,2) = ten4%abcd(2,3,2,3)
        ten4%abcd(3,2,2,3) = ten4%abcd(2,3,2,3)
        ten4%abcd(3,2,3,2) = ten4%abcd(2,3,2,3)
        
        ten4%abcd(3,1,3,1) = 0.25 *array_6x6(6,6) 
        ten4%abcd(3,1,1,3) = ten4%abcd(3,1,3,1)
        ten4%abcd(1,3,3,1) = ten4%abcd(3,1,3,1)
        ten4%abcd(1,3,1,3) = ten4%abcd(3,1,3,1)

        do i=1,3
            ten4%abcd(i,i,1,2) = array_6x6(i,4) 
            ten4%abcd(i,i,2,1) = ten4%abcd(i,i,1,2)
            
            ten4%abcd(i,i,2,3) = array_6x6(i,5) 
            ten4%abcd(i,i,3,2) = ten4%abcd(i,i,2,3) 
            
            ten4%abcd(i,i,3,1) = array_6x6(i,6) 
            ten4%abcd(i,i,1,3) = ten4%abcd(i,i,3,1) 
            
            ten4%abcd(1,2,i,i) = array_6x6(4,i)
            ten4%abcd(2,1,i,i) = ten4%abcd(1,2,i,i)

            ten4%abcd(2,3,i,i) = array_6x6(5,i)
            ten4%abcd(3,2,i,i) = ten4%abcd(2,3,i,i)

            ten4%abcd(3,1,i,i) = array_6x6(6,i) 
            ten4%abcd(1,3,i,i) = ten4%abcd(3,1,i,i)
        enddo
        
        ten4%abcd(1,2,2,3) = array_6x6(4,5) 
        ten4%abcd(1,2,3,2) =  ten4%abcd(1,2,2,3)
        ten4%abcd(2,1,2,3) =  ten4%abcd(1,2,2,3)
        ten4%abcd(1,2,3,2) =  ten4%abcd(1,2,2,3)

        ten4%abcd(1,2,3,1) = array_6x6(4,6) 
        ten4%abcd(1,2,1,3) = ten4%abcd(1,2,3,1)
        ten4%abcd(2,1,3,1) = ten4%abcd(1,2,3,1)
        ten4%abcd(2,1,1,3) = ten4%abcd(1,2,3,1)
        
        ten4%abcd(2,3,1,2) = array_6x6(5,4) 
        ten4%abcd(2,3,2,1) = ten4%abcd(2,3,1,2)
        ten4%abcd(3,2,1,2) = ten4%abcd(2,3,1,2)
        ten4%abcd(3,2,2,1) = ten4%abcd(2,3,1,2)

        ten4%abcd(2,3,3,1) = array_6x6(5,6) 
        ten4%abcd(2,3,1,3) = ten4%abcd(2,3,3,1)
        ten4%abcd(3,2,3,1) = ten4%abcd(2,3,3,1)
        ten4%abcd(3,2,1,3) = ten4%abcd(2,3,3,1)
        
        ten4%abcd(3,1,1,2) = array_6x6(6,4) 
        ten4%abcd(3,1,2,1) = ten4%abcd(3,1,1,2)
        ten4%abcd(1,3,1,2) = ten4%abcd(3,1,1,2)
        ten4%abcd(1,3,2,1) = ten4%abcd(3,1,1,2)      
        
        ten4%abcd(3,1,2,3) = array_6x6(6,5) 
        ten4%abcd(3,1,3,2) = ten4%abcd(3,1,2,3)
        ten4%abcd(1,3,2,3) = ten4%abcd(3,1,2,3)
        ten4%abcd(1,3,3,2) = ten4%abcd(3,1,2,3)  
        
       array_6x6_to_ten_4  = ten4
        
      end function array_6x6_to_ten_4