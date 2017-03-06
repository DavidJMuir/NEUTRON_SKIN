!-------------------------------------------------------------------------------
! TITLE: NEUTRON SKIN
! AUTHOR: DAVID MUIR
! DATE: 01/03/2017 - --/--/--
!-------------------------------------------------------------------------------
!                                  DESCRIPTION
!
!
!-------------------------------------------------------------------------------

PROGRAM NEUTRON_SKIN
  USE PARAMETERS
  USE ANALYSIS

  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: FUNCTIONAL_NEUTRON_SKIN_2pF
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: FUNCTIONAL_NEUTRON_SKIN_QUANTUM
  REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: FUNCTIONAL_NEUTRON_SKIN_HELM

  INTEGER :: i
  ALLOCATE(FUNCTIONAL_NEUTRON_SKIN_2pF(NUMBER_OF_FUNCTIONALS))
  ALLOCATE(FUNCTIONAL_NEUTRON_SKIN_QUANTUM(NUMBER_OF_FUNCTIONALS))
  ALLOCATE(FUNCTIONAL_NEUTRON_SKIN_HELM(NUMBER_OF_FUNCTIONALS))

!-------------------------------------------------------------------------------
  ! TESTS AND PSEUDOCODE FOR EMAINING WORK!

  ! HOW CAN I BREAK THIS UP INTO MODULES AND SUBROUTINES.
!-------------------------------------------------------------------------------

  ! REAL(KIND=dp) :: r_p_2pF, r_n_2pF, r_p_QUANTUM, r_n_QUANTUM, r_p_HELM, r_n_HELM

  ! DO FOR LIST OF TEST NUCLEI
  ! DO FOR EACH ISOTOPE
  ! DO FOR VALUES
  ! OBTAIN THE PROTON AND NEUTRON FERMI ENERGIES AND TEST THAT THEY ARE NEGATIVE!
  !
  ! OBTAIN THE DELTA R.M.S. RADII AND COMPUTE THE NEUTRON SKIN THICKNESS FOR EACH OF THE THREE MODELS.

  ! FUNCTIONAL_NEUTRON_SKIN_2pF(i) = r_n_2pF - r_p_2pF
  ! FUNCTIONAL_NEUTRON_SKIN_QUANTUM(i) = r_n_QUANTUM - r_p_QUANTUM
  ! FUNCTIONAL_NEUTRON_SKIN_HELM(i) = r_n_HELM - r_p_HELM

  ! END DO
  ! END DO
  ! END DO
!-------------------------------------------------------------------------------

  DO i = 1, NUMBER_OF_FUNCTIONALS
     FUNCTIONAL_NEUTRON_SKIN_2pF(i) = SQRT(i*5.0_dp)
     FUNCTIONAL_NEUTRON_SKIN_QUANTUM(i) = SQRT(i*10.0_dp)
     FUNCTIONAL_NEUTRON_SKIN_HELM(i) = SQRT(i*50.0_dp)
  END DO

  PRINT*, "          2pF          QUANTUM MECHANICAL DEFINITION         HELM"

  DO i = 1, NUMBER_OF_FUNCTIONALS
     PRINT*, FUNCTIONAL_NEUTRON_SKIN_2pF(i), FUNCTIONAL_NEUTRON_SKIN_QUANTUM(i), FUNCTIONAL_NEUTRON_SKIN_HELM(i)
  END DO

  DO i = 1, BOOTSTRAP_ITERATIONS
     PRINT*, "--------------------------------------------------------"
     PRINT*, "2pF ITERATION:", i
     PRINT*, "--------------------------------------------------------"
     CALL BOOTSTRAP(FUNCTIONAL_NEUTRON_SKIN_2pF)
     PRINT*, "--------------------------------------------------------"
     PRINT*, "QUANTUM MECHANICAL DEFINITION ITERATION:", i
     PRINT*, "--------------------------------------------------------"
     CALL BOOTSTRAP(FUNCTIONAL_NEUTRON_SKIN_QUANTUM)
     PRINT*, "--------------------------------------------------------"
     PRINT*, "HELM MODEL ITERATION:", i
     PRINT*, "--------------------------------------------------------"
     CALL BOOTSTRAP(FUNCTIONAL_NEUTRON_SKIN_HELM)
  END DO

END PROGRAM NEUTRON_SKIN

