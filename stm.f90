PROGRAM stm

  ! Simple temperature model for slurry in channels or pits in a barn
  ! Date           Who                         Description
  ! 28 FEB 2015   S. Hafner                    Original code (stole some from tmad.f90)
  ! 01 MAR 2015   S. Hafner                    Added calculation of substrate temperatures and heat transfer coefficients
  !                                            First realistic version
  ! 31 MAR 2015   S. Hafner                    Added option for mechanical ventilation with heating, changed calculation of air
  !                                            temperature and substrate temperatures
  ! 08 MAY 2015   S. Hafner                    Changed name of transport_parameters.txt to transfer_parameters.txt 
  ! 26 May 2020   S. Hafner                    Create STM repo, will use Git for tracking development

  IMPLICIT NONE
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Declaration statements 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Days, and other integers
  INTEGER :: HR          ! Hour of day (1-24)
  INTEGER :: DOY         ! Day of year (1 - 365)
  INTEGER :: targetDOY   ! Day of year in target_temp.txt file
  INTEGER :: nextTargetDOY   ! Day of year in target_temp.txt file
  INTEGER :: DOS         ! Day of simulation
  INTEGER :: nDays       ! Number of days in simulation
  INTEGER :: startingDOY ! Starting day of year
  INTEGER :: hottestDOY  ! Hottest day of year
  INTEGER :: emptyDOY1   ! 
  INTEGER :: emptyDOY2   ! 
  INTEGER :: wallAvePeriod, floorAvePeriod ! Number of days in running averages for wall and floor temperature
  INTEGER :: fileStat    ! End of file indicator of target_temp.txt

  ! Simulation ID
  CHARACTER (LEN=4) :: ID ! ID code of simulation
  CHARACTER (LEN=1) :: ventType ! Type of ventilation

  ! Temperatures, all in degrees C
  REAL, DIMENSION(365) :: tempAir, tempFloor, tempWall ! Air, floor (bottom of channel or pit), and wall (side of channel or pit)
  REAL :: tempSum, dTemp
  REAL :: tempTarget     ! Target temperature for a particular period when heated
  REAL :: nextTempTarget     ! Target temperature for a particular period when heated
  REAL :: maxAnnTemp     ! Maximum daily average air temperature over the year
  REAL :: minAnnTemp     ! Minimum daily average air temperature over the year
  REAL :: tempInitial    ! Initial slurry temperature
  REAL :: tempSlurry     ! Hourly or daily slurry temperature 
  REAL :: sumTempSlurry  ! Sum of hourly slurry temperatures for calculating daily mean
  REAL :: tempIn         ! Temperature of slurry when added to channel/pit
  REAL :: trigPartTemp   ! Sine part of temperature expression

  ! Geometry of channel or pit
  REAL :: slurryDepth    ! Depth of slurry in channel/pit (m)
  REAL :: channelDepth   ! Total depth of channel/pit (m)
  REAL :: buriedDepth    ! Buried depth (m)
  REAL :: wallDepth      ! Depth at which horizontal heat transfer is evaluated (m) 
  REAL :: length, width  ! Length and width of channel/pit (m)
  REAL :: areaFloor      ! Channel or pit floor area (m2)
  REAL :: areaWall       ! Channel or pit wall area (m2)
  REAL :: areaSurf       ! Slurry upper surface area (m2)
  REAL :: areaConv       ! 
  INTEGER :: nChannels   ! Number of channels or pits

  ! Solar
  REAL :: areaSol        ! 
  REAL :: absorp         !
  REAL :: maxAnnRad      ! Maximum daily average radiation over the year
  REAL :: minAnnRad      ! Minimum daily average radiation over the year
  REAL, DIMENSION(365) :: solRad
  REAL :: trigPartRad    ! Sine part of radiation expression
  
  ! Other slurry variables
  REAL :: massSlurry     ! Total slurry mass (kg)
  REAL :: slurryVol      ! Initial slurry volume (m3) NTS not consistent name
  REAL :: slurryProd     ! Slurry production rate (= inflow = outflow) (tonnes/d)

  ! Heat transfer coefficients and related variables
  REAL :: kCAir          ! Convective heat transfer coefficient W/m2-K (J/s-m2-K) 
  REAL :: kSlur          ! Thermal conductivity of slurry in W/m-K
  REAL :: kConc          ! Thermal conductivity of concrete in W/m-K
  REAL :: cpConc         ! Heat capacity of concrete J/kg-K
  REAL :: cpSlurry       ! Heat capacity of slurry J/kg-K
  REAL :: dConc          ! Density of concrete kg/m3
  REAL :: dSlurry        ! Density of slurry kg/m3
  REAL :: dampDepth      ! Damping depth for soil model in m
  REAL :: glConc         ! Gradient length within concrete substrate in m
  REAL :: glSlur         ! Gradient length within slurry in m

  ! Heat flux variables in W/m2 out of slurry
  REAL :: Qrad           ! "To" sun
  REAL :: Qslur2air      ! To air
  REAL :: Qslur2wall     ! To wall
  REAL :: Qslur2floor    ! To floor
  REAL :: Qslur2eff      ! To effluent (includes flow in)
  REAL :: Qout           ! Total

  ! Other parameters
  REAL, PARAMETER :: PI = 3.1415927
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Open files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Input files
  OPEN (UNIT=1,FILE='user_parameters.txt',STATUS='OLD')
  OPEN (UNIT=2,FILE='transfer_parameters.txt',STATUS='OLD')
  OPEN (UNIT=3,FILE='target_temp.txt',STATUS='OLD')

  ! Read in simulation ID
  READ(1,*)
  READ(1,*) ID

  ! Output files, name based on ID
  OPEN (UNIT=10,FILE='temp_pred_'//ID//'.txt',STATUS='UNKNOWN')
  !OPEN (UNIT=11,FILE='dump'//ID//'.txt',STATUS='UNKNOWN')
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Read parameters
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! User parameters
  READ(1,*) nDays
  READ(1,*) startingDOY
  READ(1,*) nChannels
  READ(1,*) channelDepth
  READ(1,*) buriedDepth
  READ(1,*) length
  READ(1,*) width
  READ(1,*) areaConv
  READ(1,*) areaSol
  READ(1,*) absorp
  READ(1,*) slurryVol
  READ(1,*) minAnnTemp
  READ(1,*) maxAnnTemp 
  READ(1,*) hottestDOY
  READ(1,*) minAnnRad
  READ(1,*) maxAnnRad 
  READ(1,*) slurryProd
  READ(1,*) tempIn
  READ(1,*) emptyDOY1
  READ(1,*) emptyDOY2 
  READ(1,*) ventType

  ! Other parameters
  READ(2,*) 
  READ(2,*) tempInitial
  READ(2,*) kCAir
  READ(2,*) kSlur
  READ(2,*) kConc
  READ(2,*) cpConc
  READ(2,*) cpSlurry
  READ(2,*) dConc
  READ(2,*) dSlurry
  READ(2,*) glConc
  READ(2,*) glSlur

  ! Output file header
  WRITE(10,*) 'Day of  Day of  Slurry  Slurry  Air    Wall  Floor  Slurry'
  WRITE(10,*) 'sim.     year    mass   depth    T      T      T      T'
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initial calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Calculate some geometry-related variables
  wallDepth = 0.5 * buriedDepth
  areaFloor = width*length*nChannels
  areaSurf = width*length*nChannels
  massSlurry = slurryVol * dSlurry / 1000 ! Slurry mass is in tonnes = 1000 kg

  ! Substrate damping depth
  dampDepth = SQRT(2.*(kConc/(dConc*cpConc))*3600.*24./(2.*PI/365.))

  ! Determine airTemp, solRad, and substrate temperatures for a complete year
  ! Start day loop
  DO DOY = 1,365,1
    ! Use sine curve to determine air temp
    trigPartTemp = (maxAnnTemp - minAnnTemp)*SIN((DOY - hottestDOY)*2*PI/365. + 0.5*PI)/2.
    tempAir(DOY) = trigPartTemp + (minAnnTemp + maxAnnTemp)/2.

    trigPartRad = (maxAnnRad - minAnnRad)*SIN((DOY - hottestDOY)*2*PI/365. + 0.5*PI)/2.
    solRad(DOY) = trigPartRad + (minAnnRad + maxAnnRad)/2.
  END DO

  ! If heated, correct air temperature if it is colder than target 
  ! Skip header
  READ(3,*) 
  IF (ventType == 'H') THEN

    READ(3,*) targetDOY,tempTarget
    READ(3,*) nextTargetDOY,nextTempTarget

    DO DOY = 1,365,1

      IF (DOY >= nextTargetDOY) THEN
        tempTarget = nextTempTarget
        IF (.NOT. IS_IOSTAT_END(fileStat)) THEN
          READ(3,*,IOSTAT=fileStat) nextTargetDOY,nextTempTarget
        ELSE
          nextTargetDOY = 366
        END IF
      END IF
      
      tempAir(DOY) = MAX(tempAir(DOY),tempTarget)

    END DO
  END IF

  ! Substrate temperature based on moving average
  ! NTS: how about aveperiods > 365?
  wallAvePeriod = wallDepth/3.0*365
  floorAvePeriod = buriedDepth/3.0*365

  ! Calculate moving average for first day of year
  ! Wall first
  IF (wallAvePeriod > 365) THEN
    DO DOY = 1,365,1
      tempWall(DOY) = (minAnnTemp + maxAnnTemp)/2.
    END DO
  ELSE 
    ! First need to calculate value for DOY = 1
    tempWall(1) = 0
    DO DOY = 365 - wallAvePeriod + 2,365,1
      tempWall(1) = tempWall(1) + tempAir(DOY)/wallAvePeriod
    END DO

    DO DOY = 2,365,1
      IF (DOY > wallAvePeriod) THEN
        tempWall(DOY) = tempWall(DOY - 1) - tempAir(DOY - wallAvePeriod)/wallAvePeriod + tempAir(DOY)/wallAvePeriod
      ELSE 
        tempWall(DOY) = tempWall(DOY - 1) - tempAir(365 + DOY - wallAvePeriod)/wallAvePeriod + tempAir(DOY)/wallAvePeriod
      END IF 
    END DO
  END IF

  ! Then floor
  IF (floorAvePeriod > 365) THEN
    DO DOY = 1,365,1
      tempFloor(DOY) = (minAnnTemp + maxAnnTemp)/2.
    END DO
  ELSE 
    ! First need to calculate value for DOY = 1
    tempFloor(1) = 0
    DO DOY = 365 - floorAvePeriod + 2,365,1
      tempFloor(1) = tempFloor(1) + tempAir(DOY)/floorAvePeriod
    END DO

    DO DOY = 2,365,1
      IF (DOY > floorAvePeriod) THEN
        tempFloor(DOY) = tempFloor(DOY - 1) - tempAir(DOY - floorAvePeriod)/floorAvePeriod + tempAir(DOY)/floorAvePeriod
      ELSE 
        tempFloor(DOY) = tempFloor(DOY - 1) - tempAir(365 + DOY - floorAvePeriod)/floorAvePeriod + tempAir(DOY)/floorAvePeriod
      END IF 
    END DO
  END IF

  ! Set initial slurry temperature
  tempSlurry = tempInitial

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Start simulation
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Start main day loop
  DOY = startingDOY - 1
  DO DOS = 1,nDays,1

    DOY = DOY + 1
    IF (DOY == 366) THEN
      DOY = 1
    END IF

    ! Empty and add slurry at beginning of day
    IF (DOY == emptyDOY1 .OR. DOY == emptyDOY2) THEN
      massSlurry = 0
    END IF

    ! Update slurry mass and temperature from addition
    tempSlurry = (tempSlurry * massSlurry + tempIn * slurryProd)/(massSlurry + slurryProd)
    massSlurry = massSlurry + slurryProd

    ! Get depth and wall area
    slurryDepth = 1000 * massSlurry / (dSlurry*width*length*nchannels)  ! Slurry depth in m
    areaWall = slurryDepth*2.*(length + width)*nChannels

    ! Start hour loop
    sumTempSlurry = 0

    ! Radiation fixed for day (W = J/s)
    Qrad = - solRad(DOY) * areaSol

    DO HR = 1,24,1
    
      ! Calculate heat transfer rates, all in Watts (J/s)
      ! WIP NTS set limit in input file
      Qslur2air = kCAir*(tempSlurry - tempAir(DOY))*areaConv
      Qslur2floor = 1./(glConc/kConc + glSlur/kSlur)*(tempSlurry - tempFloor(DOY))*areaFloor
      Qslur2wall = 1./(glConc/kConc + glSlur/kSlur)*(tempSlurry - tempWall(DOY))*areaWall
      Qout = Qrad + Qslur2air + Qslur2wall + Qslur2floor

      ! Update slurry temperature
      dTemp = Qout*3600./(cpSlurry*massSlurry)

      IF (dTemp < -0.1) THEN
        dTemp = -0.1
      ELSE IF (dTemp > 0.1) THEN
        dTemp = 0.1
      END IF 

      tempSlurry = tempSlurry - dTemp
      sumTempSlurry = sumTempSlurry + tempSlurry

    END DO


    WRITE(10,"(1X,I4,5X,I3,1X,7F7.1,5X,F15.1)") DOS, DOY, massSlurry, slurryDepth, tempAir(DOY), tempWall(DOY), tempFloor(DOY), &
      & sumTempSlurry/24., Qout 

  END DO

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Close files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CLOSE(2)
  CLOSE(10)
  CLOSE(11)

  STOP
END PROGRAM stm
