       identification division.
       program-id. DataSplitCount.
       author. Jaime Barillas.
       date-written. 2019-04-17.
      *Desc: Report the totals of each of the classes of records. Also
      *split the sales and layaways records from the returns.

       environment division.
       input-output section.
       file-control.
           select input-file
               assign to 'data/valid.out'
               organization is line sequential.

           select sales-layaways-file
               assign to 'data/slrecords.dat'
               organization is line sequential.

           select returns-file
               assign to 'data/rrecords.dat'
               organization is line sequential.

           select report-file
               assign to 'data/counts-and-control-totals.out'
               organization is line sequential.


       configuration section.


       data division.
       file section.
           fd input-file
               data record is input-record
               record contains 36 characters.

           01 input-record.
               05 in-trans-code        pic x.
               05 in-trans-amnt        pic 9(5)v99.
               05 in-pay-type          pic xx.
               05 in-store-num         pic 99.
               05 in-invoice-num       pic x(9).
               05 in-sku-code          pic x(15).


           fd sales-layaways-file
               data record is sl-output
               record contains 36 characters.

           01 sl-output                pic x(36).

           fd returns-file
               data record is r-output
               record contains 36 characters.

           01 r-output                 pic x(36).

           fd report-file
               data record is report-output
               record contains 120 characters.

           01 report-output            pic x(120).


       working-storage section.

       01 ws-eof-flag                  pic x value 'n'.

       01 ws-trans-code                pic x value space.
           88 ws-sale-or-layaway             value 'S', 'L'.


       procedure division.
       000-main.
           open input input-file,
               output sales-layaways-file,
                      returns-file,
                      report-file.

           read input-file
               at end move 'y'         to ws-eof-flag.

           perform 100-process-record
             until ws-eof-flag = 'y'.

           close input-file,
                 sales-layaways-file,
                 returns-file,
                 report-file.
           goback.


       100-process-record.
           move in-trans-code          to ws-trans-code.

           perform 500-print-record.

           read input-file
               at end move 'y'         to ws-eof-flag.


       500-print-record.
           if (ws-sale-or-layaway)
               write sl-output         from input-record
           else
               write r-output          from input-record
           end-if.

       end program DataSplitCount.