      **************************************
      *****     όoΙέΟt@C     *****
      **************************************
       01  JNSR.
           02  JNSR_PNAME1            PIC  X(005) VALUE "JNSR1".
           02  F                      PIC  X(001).
           02  JNSR_PNAME2            PIC  X(005) VALUE "JNSR2".
           02  F                      PIC  X(001).
           02  JNSR_PNAME3            PIC  X(005) VALUE "JNSR3".
           02  F                      PIC  X(001).
           02  JNSR_LNAME             PIC  X(004) VALUE "JNSR".
           02  F                      PIC  X(001).
           02  JNSR_KEY1              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY2              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY3              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY4              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY5              PIC  X(100) VALUE SPACE.
           02  JNSR_SORT              PIC  X(100) VALUE SPACE.
           02  JNSR_IDLST             PIC  X(100) VALUE SPACE.
           02  JNSR_RES               USAGE  POINTER.
       01  JNSR-R.
           02  JNSR-KEY1.                                               KEY1
             03  JNSR-01              PIC  9(06)  COMP-3.               iΌΊ°Δή
             03  JNSR-02              PIC  9(08)  COMP-3.
             03  JNSR-03              PIC  9(02).                       όoΝζ
             03  JNSR-04              PIC  9(06)  COMP-3.               `[
             03  JNSR-05              PIC  9(01).                       s
           02  JNSR-06                PIC  9(01).                       qΊ°Δή
           02  JNSR-07                PIC  9(01).                       TCY
           02  JNSR-08.                                                 όoΙ
             03  JNSR-081             PIC S9(04)  COMP-3    OCCURS  10.
           02  JNSR-09                PIC  9(01).                       ΆYζͺ
           02  JNSR-10                PIC  9(01).                       oΧ`ζ
           02  JNSR-11.
             03  JNSR-111             PIC  9(04).                       ΎΣζC
             03  JNSR-112             PIC  9(03).                       ΌNO
           02  JNSR-12                PIC  9(06)  COMP-3.               θσ
           02  JNSR-13                PIC  9(01).                       aθζͺ
           02  JNSR-14                PIC  9(01).                       ^C
           02  JNSR-KEY2.
             03  JNSR-15.                                               σ
               04  JNSR-151           PIC  9(06)  COMP-3.               @σ
               04  JNSR-152           PIC  9(01).                       @s
             03  JNSR-16              PIC  9(08)  COMP-3.
             03  JNSR-17              PIC  9(01).                       ΪΊ°ΔήKBN
             03  JNSR-18.                                               `[
               04  JNSR-181           PIC  9(06)  COMP-3.               @`[
               04  JNSR-182           PIC  9(01).                       @s
           02  JNSR-KEY3.
             03  JNSR-19              PIC  9(04).                       ΎΣζC
             03  JNSR-20              PIC  9(08)  COMP-3.
             03  JNSR-21              PIC  9(01).                       R[ζ
             03  JNSR-22.                                               `[
               04  JNSR-221           PIC  9(06)  COMP-3.               oΧw
               04  JNSR-222           PIC  9(01).                       @s
           02  JNSR-23                PIC  N(09).                       zB
           02  JNSR-24                PIC  N(23).                       Ev
           02  JNSR-81                PIC  9(08)  COMP-3.
           02  JNSR-82                PIC  9(03).
           02  JNSR-90                PIC  9(01).                       όΝ
           02  JNSR-91                PIC  9(01).                       Jzζͺ
           02  JNSR-92                PIC  9(02).                       
       77  F                          PIC  X(001).
