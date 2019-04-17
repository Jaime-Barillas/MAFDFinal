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
           88 ws-trans-sale                  value 'S'.
           88 ws-trans-layaway               value 'L'.
           88 ws-trans-return                value 'R'.

       01 ws-report-heading.
           05 filler                   pic x(40)
               value ' Counts And Control Totals -- Final Gr 8'.

       01 ws-totals.
           05 ws-tot-sl-rec            pic 9(4)
               value 0.
           05 ws-tot-sl-rec-amnt       pic 9(9)v99
               value 0.
           05 ws-tot-s-rec             pic 9(4)
               value 0.
           05 ws-tot-s-rec-amnt        pic 9(9)v99
               value 0.
           05 ws-tot-l-rec             pic 9(4)
               value 0.
           05 ws-tot-l-rec-amnt        pic 9(9)v99
               value 0.
           05 ws-tot-s-rec-ptc         pic 999v99
               value 0.
           05 ws-tot-l-rec-ptc         pic 999v99
               value 0.
           05 ws-tot-r-rec             pic 9(4)
               value 0.
           05 ws-tot-r-rec-amnt        pic 9(9)v99
               value 0.

       01 ws-td-sl-records.
           05 filler                   pic x value space.
           05 filler                   pic x(27)
               value 'Total S&L Records:         '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-sl-rec             pic Z,ZZ9.

       01 ws-td-sl-amount.
           05 filler                   pic x value space.
           05 filler                   pic x(18)
               value 'Total S&L Amount: '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-sl-rec-amnt        pic ZZZ,ZZZ,ZZ9.99.

       01 ws-td-s-records.
           05 filler                   pic x value space.
           05 filler                   pic x(27)
               value 'Total S Records:           '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-s-rec              pic Z,ZZ9.

       01 ws-td-s-amount.
           05 filler                   pic x value space.
           05 filler                   pic x(18)
               value 'Total S Amount:   '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-s-rec-amnt         pic ZZZ,ZZZ,ZZ9.99.

       01 ws-td-l-records.
           05 filler                   pic x value space.
           05 filler                   pic x(27)
               value 'Total L Records:           '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-l-rec              pic Z,ZZ9.

       01 ws-td-l-amount.
           05 filler                   pic x value space.
           05 filler                   pic x(18)
               value 'Total L Amount:   '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-l-rec-amnt         pic ZZZ,ZZZ,ZZ9.99.

       01 ws-td-s-percent.
           05 filler                   pic x value space.
           05 filler                   pic x(26)
               value 'Percent S:                '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-s-rec-pct          pic ZZ9.9.
           05 filler                   pic x
               value '%'.

       01 ws-td-l-percent.
           05 filler                   pic x value space.
           05 filler                   pic x(26)
               value 'Percent R:                '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-l-rec-pct          pic ZZ9.9.
           05 filler                   pic x
               value '%'.

       01 ws-td-r-records.
           05 filler                   pic x value space.
           05 filler                   pic x(27)
               value 'Total R Records:           '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-r-rec              pic Z,ZZ9.

       01 ws-td-r-amount.
           05 filler                   pic x value space.
           05 filler                   pic x(18)
               value 'Total R Amount:   '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-r-rec-amnt         pic ZZZ,ZZZ,ZZ9.99.


       procedure division.
       000-main.
           open input input-file,
               output sales-layaways-file,
                      returns-file,
                      report-file.

           read input-file
               at end move 'y'         to ws-eof-flag.

           perform 100-print-headings.
           perform 200-accumulate-totals
             until ws-eof-flag = 'y'.
           perform 300-calculate-percentages.
           perform 400-print-totals.

           close input-file,
                 sales-layaways-file,
                 returns-file,
                 report-file.
           goback.


       100-print-headings.
           write report-output         from ws-report-heading
               before advancing 2 lines.


       200-accumulate-totals.
           move in-trans-code          to ws-trans-code.

           if (ws-trans-sale)
               add 1                   to ws-tot-sl-rec
               add in-trans-amnt       to ws-tot-sl-rec-amnt

               add 1                   to ws-tot-s-rec
               add in-trans-amnt       to ws-tot-s-rec-amnt
           else if (ws-trans-layaway)
               add 1                   to ws-tot-sl-rec
               add in-trans-amnt       to ws-tot-sl-rec-amnt

               add 1                   to ws-tot-l-rec
               add in-trans-amnt       to ws-tot-l-rec-amnt
           else if (ws-trans-return)
               add 1                   to ws-tot-r-rec
               add in-trans-amnt       to ws-tot-r-rec-amnt
           end-if
           end-if
           end-if.

      *    Lastly, we print the record to the appropriate file.
           perform 250-print-record.

           read input-file
               at end move 'y'         to ws-eof-flag.


       250-print-record.
           if (ws-trans-sale or ws-trans-layaway)
               write sl-output         from input-record
           else
               write r-output          from input-record
           end-if.


       300-calculate-percentages.
           compute ws-tot-s-rec-ptc rounded =
             (ws-tot-s-rec / ws-tot-sl-rec) * 100.
           compute ws-tot-l-rec-ptc rounded =
             (ws-tot-l-rec / ws-tot-sl-rec) * 100.


       400-print-totals.
           move ws-tot-sl-rec          to ws-td-sl-rec.
           move ws-tot-sl-rec-amnt     to ws-td-sl-rec-amnt.
           move ws-tot-s-rec           to ws-td-s-rec.
           move ws-tot-s-rec-amnt      to ws-td-s-rec-amnt.
           move ws-tot-l-rec           to ws-td-l-rec.
           move ws-tot-l-rec-amnt      to ws-td-l-rec-amnt.
           move ws-tot-r-rec           to ws-td-r-rec.
           move ws-tot-r-rec-amnt      to ws-td-r-rec-amnt.
           move ws-tot-s-rec-ptc       to ws-td-s-rec-pct.
           move ws-tot-l-rec-ptc       to ws-td-l-rec-pct.

           write report-output         from ws-td-sl-records.
           write report-output         from ws-td-sl-amount.
           write report-output         from ws-td-s-records.
           write report-output         from ws-td-s-amount.
           write report-output         from ws-td-l-records.
           write report-output         from ws-td-l-amount.
           write report-output         from ws-td-r-records.
           write report-output         from ws-td-r-amount.
           write report-output         from ws-td-s-percent.
           write report-output         from ws-td-l-percent
               before advancing 2 lines.

       end program DataSplitCount.
