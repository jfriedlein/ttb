! -----------MODULE TENSORXKINEMATICS----------------------------
     
      module TensorXkinematics
       use Tensor 
       use TensorXLSDYNA
       ! ---------------------------------------
       ! 
       ! ---------------------------------------
       !
       ! ---------------------------------------
       ! use this module in the following form:
       ! ---------------------------------------
       ! 
       ! ---------------------------------------
       
      
!      ------BEGIN INTERFACE-------------------------------------
       interface vol
       module procedure vol_2
       module procedure vol_2s
       end interface
         
       interface array_to_ten
       module procedure array_6x6_to_ten_4
       end interface
       
       interface inv
       module procedure inv_4
       end interface
       
       interface symmetrize
       module procedure symmetrize_2
       end interface
       
       interface stress_push_forward
       module procedure stress_push_forward_2
       end interface
       
       interface tangent_push_forward
       module procedure tangent_push_forward_4
       end interface
       
       interface deviatoric_I4
       module procedure dev_I4
       !module procedure dev_I4s
       end interface    
!      ------END INTERFACE---------------------------------------
       
       contains

!      ------BEGIN FUNCTIONS-------------------------------------
!      ------TENSOR FUNCTION SECTION-----------------------------
       include './libvol.f'
       include './libsym.f'
       include './libinv.f'
       include './liblist_to_ten.f'
       include './libpush_forward.f'
!      ------TENSOR INITIALIZATION SECTION-----------------------
       include './libdeviatoricI.f'
!      ------END FUNCTIONS---------------------------------------

       end module TensorXkinematics