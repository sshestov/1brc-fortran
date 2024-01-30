Program test_mmap_file

use iso_c_binding
use, intrinsic :: iso_fortran_env, only : iostat_end

implicit none

interface

integer function open(pathname,flags) bind(c,name='open')
  use iso_c_binding
  !type(c_ptr), intent(in) :: pathname
  type(c_ptr), value, intent(in) :: pathname
  integer(c_int), value :: flags
end function open

integer function close(fd) bind(c,name='close')
  use iso_c_binding
  integer(c_int), value :: fd
end function close

type(c_ptr) function mmap(addr,len,prot,flags,fildes,off) bind(c,name='mmap')
  use iso_c_binding
  integer(c_int), value :: addr
  integer(c_size_t), value :: len
  integer(c_int), value :: prot
  integer(c_int), value :: flags
  integer(c_int), value :: fildes
  integer(c_size_t), value :: off
end function mmap

integer function munmap(addr, len) bind(c,name='munmap')
  use iso_c_binding
  type(c_ptr), value :: addr
  integer(c_size_t), value :: len
end function munmap

type(c_ptr) function invoke_mmap(addr,len,prot,flags,fildes,off) bind(c,name='invoke_mmap')
  use iso_c_binding
  integer(c_int), value :: addr
  !type(c_ptr), value :: addr
  integer(c_size_t), value :: len
  integer(c_int), value :: prot
  integer(c_int), value :: flags
  integer(c_int), value :: fildes
  integer(c_size_t), value :: off
end function invoke_mmap

integer function mopen(pathname, flags) bind(c,name='mopen')
  use iso_c_binding
  type(c_ptr), value :: pathname
  integer(c_int), value :: flags
end function mopen

end interface



!Here are the variables used to make the call to mmap.
type(c_ptr) :: cptr
integer(c_size_t) :: flen, off
integer,parameter :: PROT_READ=1, MAP_PRIVATE=2, O_RDONLY=0
integer(c_int) :: fd
character,       dimension(:), pointer, contiguous :: x11
integer(kind=1), dimension(:), pointer, contiguous :: i11

integer, parameter :: ikind=8
character(len=100) :: str
integer(ikind) :: iostat, i, j, pos_s, pos_nl, cpos, pos_s1
integer(ikind) :: id, chunk_size, chunk_num
character :: nl_t=''
real  :: temp

integer, parameter :: Nsts=65535, Nchunks=16
character(len=40), dimension(Nsts) :: stations=''
integer(kind=1),   dimension(1:40,Nsts) :: stations_i=0
real,              dimension(Nsts) :: temp_min=1e+4, temp_max=-1e+4, temp_cum=0.0
integer,           dimension(Nsts) :: temp_min_i=1000, temp_max_i=-1000, temp_cum_i=0
integer,           dimension(Nsts) :: stations_cnt=0
integer(ikind), dimension(Nchunks) :: ch_start,ch_end                              ! absolute positions of starts and ends of i-th chunk
integer                            :: d2p, d3p, d4p, m1p                           ! int yes/no of the dot position is 2nd, 3rd, 4th place in the string,
                                                                                   !    and the minus is 1st place. 
integer                            :: tempi
integer(kind=1)                    :: zcode, scode, nlcode, dcode, mcode           ! 8bit ints of symbols
integer(kind=1), dimension(16)     :: name_i16
integer(kind=1), dimension(8)      :: name_i8
integer(kind=ikind)                :: Nparts, K
character(len=16), target          :: filename='measurements.txt'
character(len=20), target          :: c_filename
type(c_ptr)                        :: c_filename_ptr

print'(A, /, 17(" "), I4, I6, I8, I8, I6, I7)', "Storage sizes for char, int(1), int(2), int, int(4), real [bits]: ", &
        storage_size(nl_t), storage_size(10_1), storage_size(10_2), storage_size(10), storage_size(10_4), storage_size(temp)
zcode = transfer('0',1_1)  ;  scode=transfer(';',1_1)  ;  dcode=transfer('.',1_1)  ;  mcode=transfer('-',1_1)  ;  nlcode=transfer(NEW_LINE(nl_t),1_1)
write(*,'(A,I3,I3,I3,I3)')  "Byte codes of '0', ';', '.', '-', and NEW_LINE are: ", zcode, scode, dcode, mcode, nlcode

fd=10
inquire( file='measurements.txt', iostat=iostat, size=flen)
print*, "File 'measurements.txt' has size ", flen, " bytes"

c_filename = trim(filename)//c_null_char
c_filename_ptr = c_loc(c_filename)

!flen = 2039 !2040
off = 0

print*, "******************* Using C functions to open and mmap the file *******************"
!cptr = invoke_mmap(0,flen,PROT_READ,MAP_PRIVATE,fd,off)
!fd = mopen(c_filename_ptr,O_RDONLY)  ;  print*, "C-filename is ***", trim(c_filename), "***", len(c_filename), len_trim(c_filename)
fd = open(c_loc(c_filename),O_RDONLY)
write(*,'(A, I4)') "The C file descriptor is ", fd
cptr = mmap(0,flen,PROT_READ,MAP_PRIVATE,fd,off)
fd = close(fd)
print*, "**********  Here we have mmap-ed the file in C and now doing Fortran stuff **********"
print*, ""

if (.not. c_associated(cptr)) print*, "cptr DOES NOT point to anything :("
call c_f_pointer(cptr,x11,[flen])   ;   if (.not. associated(x11)) print*, "x11 IS NOT associated with anything :("
call c_f_pointer(cptr,i11,[flen])   ;   if (.not. associated(i11)) print*, "i11 IS NOT associated with anything :("

write(*,'(A,I3,A)') "*************** Splitting the file in ", Nchunks, " chunks. Each OMP thread should receive its chunk ***************"
chunk_size=flen / Nchunks  ;  cpos=1
do i=1,Nchunks-1                                                                         
  ch_end(i) = i*chunk_size  +cpos-1
  pos_nl = findloc(x11(ch_end(i)-40:ch_end(i)),new_line(nl_t),DIM=1,BACK=.true.)     ! position of the last new_line symbol in the 41-char array
  ch_end(i) = ch_end(i)-41+(pos_nl)                                                   
  ch_start(i+1) = ch_end(i)+1
end do
ch_start(1)=1
ch_end(Nchunks)=flen
ch_end(Nchunks)=findloc(i11(flen-100:),scode,DIM=1)-1 + flen-100
print '(A,*(I13))', "Start: ",ch_start
print '(A,*(I13))', "Stop:  ",ch_end
!do i=1,Nchunks
!  print*, ch_start(i), x11(ch_start(i):ch_start(i)+15)
!  print*, ch_end(i), x11(ch_end(i)-9:ch_end(i))
!end do


pos_s=1 ; pos_nl=0 ; i=1 ;  j=1 ; iostat=1 !; cpos = 1
write(*,*) "******************************************* Running ******************************************* "
!$OMP PARALLEL DO REDUCTION(min:temp_min_i) REDUCTION(max:temp_max_i) REDUCTION(+:temp_cum_i,stations_cnt) PRIVATE(cpos,pos_s,pos_nl,str,i,id,temp,tempi,d2p,d3p,d4p,m1p,name_i16) DEFAULT(shared) SCHEDULE(DYNAMIC)
do chunk_num=1,Nchunks
  cpos = ch_start(chunk_num)
  do 
    pos_s=findloc(i11(cpos:),scode,DIM=1)-1      !! now pos_s means offset and ranges 0..??; before it was 1..?? without -1 in definition
    !pos_s = findsc(i11(cpos:cpos+63))-1
    !pos_s1 = findsc_ch(x11(cpos:cpos+63))-1
    !write(*,'(A, I4)',advance='NO') "Position of ';' (OLD:NEW): ", pos_s
    !write(*,'(I4)') pos_s1
    !if (pos_s /= pos_s1) print*, x11(cpos:cpos+pos_s-1) , "<-->", x11(cpos:cpos+pos_s1-1)
    d2p = merge(1,0,i11(cpos+pos_s+2) == dcode)  ;  d3p = merge(1,0,i11(cpos+pos_s+3) == dcode)  ;  d4p = merge(1,0,i11(cpos+pos_s+4) == dcode)  ;  m1p = merge(1,0,i11(cpos+pos_s+1) == mcode)
    pos_nl = d2p*(pos_s+4) + d3p*(pos_s+5) + d4p*(pos_s+6)                         !! offset of the NEW_LINE symbol counted from the cpos
                                                            
    tempi = d2p*(                                           (i11(cpos+pos_s+1)-zcode)*10 + (i11(cpos+pos_s+3)-zcode)  ) + &   ! dot is the 2nd symbol => temp has a form of 1.2
            d3p*( (1.-m1p)*((i11(cpos+pos_s+1)-zcode)*100 + (i11(cpos+pos_s+2)-zcode)*10 + (i11(cpos+pos_s+4)-zcode))   + &   ! dot is the 3rd symbol => temp has a form of 12.4 or -4.5 
                      m1p *(                              - (i11(cpos+pos_s+2)-zcode)*10 - (i11(cpos+pos_s+4)-zcode)) ) + &
            d4p*(          -(i11(cpos+pos_s+2)-zcode)*100 - (i11(cpos+pos_s+3)-zcode)*10 - (i11(cpos+pos_s+5)-zcode)  )       ! dot is the 4th symbol => temp has the form of -21.5
    !write(*,'(I5,":  ",*(A1))',advance='NO') cpos+pos_s, x11(cpos:cpos+pos_s-1)       !! exact position of the station name is cpos:cpos+pos_s-1
    !write(*,'(" : ",*(A1))',advance='NO') x11(cpos+pos_s+1:cpos+pos_nl-1), "|"        !! exact position of the temperature is cpos+pos_s+1:cpos+pos_nl-1
    !write(*,'(I6, I4, I4, I4, I4)',advance='YES') tempi, d2p, d3p, d4p, m1p
    !!write(*,*) trim(str), "<-->", temp
    
    id = get_hash_i( i11(cpos:cpos+pos_s-1) )
    if (stations_cnt(id) == 0) then       !!! old version: working with string-names
      do i=cpos,cpos+pos_s-1
        stations(id)(1+i-cpos:1+i-cpos) = x11(i)
      end do
    end if
    temp_min_i(id) = min(temp_min_i(id),tempi)
    temp_max_i(id) = max(temp_max_i(id),tempi)
    temp_cum_i(id) = temp_cum_i(id)+(tempi)
    stations_cnt(id) = stations_cnt(id)+1
    cpos = cpos+pos_nl+1
    if (cpos >= ch_end(chunk_num)) exit
  end do
end do
!$OMP END PARALLEL DO

tempi=munmap(cptr, flen)

temp_min=real(temp_min_i)/10.0
temp_max=real(temp_max_i)/10.0
temp_cum=real(temp_cum_i)/10.0


tempi=0   ! to count the full number of stations 
open(newunit=fd, file = "results.txt",status='REPLACE', iostat=iostat, iomsg=str, access='stream',action='write', form='FORMATTED')
do i=1,Nsts
  if (stations_cnt(i) .gt. 0) then
    write(fd,'(I5,"/ 0x",Z4.4, A, A20, F6.1, F6.1, F6.1, "  count ", I7)') i,i, ": ", stations(i), temp_min(i), temp_cum(i)/stations_cnt(i), temp_max(i), stations_cnt(i)
    tempi=tempi+1
  endif
end do 
write(fd,'(A,I5)') "Total numner of stations ", tempi
close(fd)

write(*,'(A,I5)') "Total numner of stations ", tempi

contains

!!! these two are needed for nvfortran, since it has no intrinsics
!pure integer function shiftr(i,n)
!  integer, value :: i, n
!  shiftr = rshift(i,n)
!end function
!
!pure integer function shiftl(i,n)
!  integer, value :: i, n
!  shiftl = lshift(i,n)
!end function

!example from https://github.com/imonlyfourteen/1brc/blob/main/1brc.c
!    uint16_t id=0;
!    /* hash compute, unroll part of the loop /* hash compute, unroll part of the loop */
!    #define ROL(x, n)  (x<<n | x>>(16-n))          // | - bitwise OR
!    id = ROL(id, 2) ^ *p++;                        // ^ - bitwise XOR
!    id = ROL(id, 2) ^ *p++;
!    id = ROL(id, 2) ^ *p++;
!    while (*p != ';') {
!        id = ROL(id, 2) ^ *p++;    }

!!integer function get_hash(array) result(res)
!!  character, dimension(:), intent(in) :: array
!!  integer :: len, i
!!  integer(2) :: ha
!!
!!  ha=0
!!  !a hash function from github below. convert char to 8-bit integer 1_1; int(X,2) - convert it to 16-bit integer
!!  do i=1,size(array)
!!    ha = ieor( ior(shiftl(ha,2),shiftr(ha,14)) , int(transfer(array(i),1_1),2) )
!!  end do 
!!  ha = shiftr(shiftl(ha,3),3)
!!  res=ha
!!end function get_hash

integer function get_hash_i(array) result(res)
  integer(kind=1), dimension(:), intent(in) :: array
  integer :: len, i
  integer(2) :: ha

  ha=21845  ! 0x5555=21845; the 0x5555 is suggested by the author
  do i=1,size(array)
    ha = ieor( ior(ishftc(ha,2),ishftc(ha,-14)) , int(array(i),2) )
  end do 
  !res = merge(int(ha,kind=4),65536-int(not(ha),kind=4)-1,ha>=0)  !! int(2) is signed; converting to unsigned int(4)
  res = 32766+ha
end function get_hash_i

integer function get_hash_i16(array) result(res)
  integer(kind=1), dimension(1:16), intent(in) :: array
  integer :: i
  integer(2) :: ha
  ha=21845  ! 0x5555=21845; the 0x5555 is suggested by the author
  do i=1,16
    ha = ieor( ior(ishftc(ha,2),ishftc(ha,-14)) , int(array(i),2) )
  end do 
  res = 32766+ha
end function get_hash_i16

integer function get_hash_i8(array) result(res)
  integer(kind=1), dimension(1:8), intent(in) :: array
  integer :: i
  integer(2) :: ha
  ha=21845  ! 0x5555=21845; the 0x5555 is suggested by the author
  do i=1,8
    ha = ieor( ior(ishftc(ha,2),ishftc(ha,-14)) , int(array(i),2) )
  end do 
  res = 32766+ha
end function get_hash_i8


!https://github.com/dannyvankooten/1brc/blob/main/3.c
!// hash returns a simple (but fast) hash for the first n bytes of data
!static unsigned int hash(const unsigned char *data, int n) {
!  unsigned int hash = 0;
!  for (int i = 0; i < n; i++) {
!    hash = (hash * 31) + data[i];
!  }
!  return hash; 
! // find index of group by key through hash with linear probing
!    int h = hash((unsigned char *)buf, pos - buf) & (HCAP - 1);            //#define HCAP=4096
!    while (map[h] != -1 && strcmp(results[map[h]].city, buf) != 0) {
!      h = (h + 1) & (HCAP - 1);
!    }
!    int c = map[h];}


! Find semicolon location in an integer array
integer function findsc(arr)
    integer(kind=1), dimension(:) :: arr  
    integer(1), parameter :: sc = 59 !iachar(';',1)
    integer(1) :: r(16)
    logical(1) :: z
    integer :: i, k 

    !! comment out since we use arr instead of unknown-length string
    !if (mod(len(str),16) /= 0) then
    !    error stop "Length should be multiple of 16"
    !end if

    do i = 1, size(arr), 16              !len(str), 16
        r = arr(i:i+15)                  !r = transfer(str(i:i+16),r)
        ! search for semicolon
        z = .false.
        !$omp simd simdlen(16) reduction(.or.: z)
        do k = 1, 16
            z = z .or. (r(k) == sc)
        end do
        if (z) then
            findsc = i + findloc(r,sc,dim=1) - 1     ! semicolon was found
            return
        end if
    end do
    findsc = 0                                       ! semicolon not found
end function

! Find semicolon location in a string
integer function findsc_ch(str)
    character(len=*) :: str
    integer(1), parameter :: sc = iachar(';',1)
    integer(1) :: r(16)
    logical(1) :: z
    integer :: i, k
    !if (mod(len(str),16) /= 0) then
    !    error stop "Length should be multiple of 16"
    !end if
    do i = 1, len_trim(str), 16
        r = transfer(str(i:i+15),r)    ! read chars into integer register
        z = .false.                    ! search for semicolon
        !$omp simd simdlen(16) reduction(.or.: z)
        do k = 1, 16
            z = z .or. (r(k) == sc)
        end do
        if (z) then                    ! semicolon was found
            !findsc_ch = i + findloc(r,sc,dim=1) - 1
            findsc_ch = i + scan(str(i:i+15),';') - 1
            return
        end if
    end do
    findsc_ch = 0                      ! semicolon not found
end function findsc_ch

end Program
