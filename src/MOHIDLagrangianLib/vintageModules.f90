
    !------------------------------------------------------------------------------
    !        IST/MARETEC, Water Modelling Group, Mohid modelling system
    !------------------------------------------------------------------------------
    !
    ! TITLE         : Mohid Model
    ! PROJECT       : Mohid Lagrangian Tracer
    ! MODULE        : vintageModules
    ! URL           : http://www.mohid.com
    ! AFFILIATION   : IST/MARETEC, Marine Modelling Group
    ! DATE          : Decenber 2019
    ! REVISION      : Canelas 0.1
    !> @author
    !> Ricardo Birjukovs Canelas
    !
    ! DESCRIPTION:
    !> Class that initts and kills vintage modules from other models
    !------------------------------------------------------------------------------

    module vintageModules_mod

    use common_modules
    use ModuleLitter

    implicit none
    private

    type :: vintageModule_class  !< The .csv parser class
        type(stringList_class) :: vintageModsNames
        integer, dimension(:), allocatable :: ModuleLitterProcID
    contains
    procedure :: initialize => initVintageModules
    procedure :: runLitterModule
    procedure :: finalize => killVintageModules
    end type vintageModule_class

    type(vintageModule_class) :: vintageModules
    
    !Public access vars
    public :: vintageModule_class, vintageModules

    contains

    !---------------------------------------------------------------------------
    !> @author Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that parses a csv file and returns the file with the data.
    !> Gets and trashes the header.
    !> @param[in] self, csvfilename, header_row
    !---------------------------------------------------------------------------
    subroutine initVintageModules(self, nBlocks)
    class(vintageModule_class), intent(inout) :: self
    integer, intent(in) :: nBlocks
    type(string) :: outext, modName
    integer :: i, procID
    integer, dimension(6) :: startDate, endDate
    real(8), dimension(4) :: bbx
    
    modName = "ModuleLitter"
    call self%vintageModsNames%add(modName)
    outext = '-> Initializing vintage module : '// modName
    call Log%put(outext)
    allocate(self%ModuleLitterProcID(nBlocks))
    startDate = Utils%getDateFromDateTime(Globals%SimTime%StartDate)
    endDate = Utils%getDateFromDateTime(Globals%SimTime%EndDate)
    do i=1, nBlocks
        bbx(1) = Globals%SimDefs%Pointmin%x
        bbx(2) = Globals%SimDefs%Pointmax%x
        bbx(3) = Globals%SimDefs%Pointmin%y
        bbx(4) = Globals%SimDefs%Pointmax%y
        procID = 0
        !call ConstructLitter(procID, startDate, bbx)
        self%ModuleLitterProcID(i) = procID
    end do
    !add more constructs here from MOHID vintage modules - maybe make simplified interface on MOHID side...
    !modName = "ModuleName"
    !call self%vintageModsNames%add(modName)
    !outext = '-> Initializing vintage module : '// modName
    !call Log%put(outext)
    
    end subroutine initVintageModules
    
    !---------------------------------------------------------------------------
    !> @author Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that parses a csv file and returns the file with the data.
    !> Gets and trashes the header.
    !> @param[in] self, csvfilename, header_row
    !---------------------------------------------------------------------------
    subroutine runLitterModule(self, procID, date, xx, yy, age, source, id, beached, killPartic)
    class(vintageModule_class), intent(inout) :: self
    integer, intent(in) :: procID
    integer, dimension(6), intent(in) :: date
    real(8), dimension(:), intent(in) :: xx
    real(8), dimension(:), intent(in) :: yy
    real(8), dimension(:), intent(in) :: age
    integer, dimension(:), intent(in) :: source
    integer, dimension(:), intent(in) :: id
    logical, dimension(:), intent(inout) :: beached
    logical, dimension(:), intent(inout) :: killPartic
    type(string) :: outext
    integer :: i
    
    !call ModifyLitter(procID,date,real(xx,8),yy,age, source,id, beached, killPartic)
    
    end subroutine runLitterModule

    !---------------------------------------------------------------------------
    !> @author Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that parses a csv file and returns the file with the data.
    !> Gets and trashes the header.
    !> @param[in] self, csvfilename, header_row
    !---------------------------------------------------------------------------
    subroutine killVintageModules(self, nBlocks)
    class(vintageModule_class), intent(inout) :: self
    integer, intent(in) :: nBlocks
    type(string) :: outext
    integer :: i
    
    call self%vintageModsNames%finalize()
    outext = '-> Finalizing vintage modules'
    call Log%put(outext)
    do i=1, nBlocks
        !call KillLitter(self%ModuleLitterProcID(i))
    end do
    !add more kills here from MOHID vintage modules
    
    end subroutine killVintageModules


    end module vintageModules_mod
