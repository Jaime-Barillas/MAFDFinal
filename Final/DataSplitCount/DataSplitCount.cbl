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
               05 in-trans-code            pic x.
               05 in-trans-amnt            pic 9(5)v99.
               05 in-pay-type              pic xx.
               05 in-store-num             pic 99.
               05 in-invoice-num           pic x(9).
               05 in-sku-code              pic x(15).

           fd sales-layaways-file
               data record is sl-output
               record contains 36 characters.

           01 sl-output                    pic x(36).

           fd returns-file
               data record is r-output
               record contains 36 characters.

           01 r-output                     pic x(36).

           fd report-file
               data record is report-output
               record contains 49 characters.

           01 report-output                pic x(49).


       working-storage section.

       01 ws-eof-flag                      pic x value 'n'.

       01 ws-trans-code                    pic x value space.
           88 ws-trans-sale                      value 'S'.
           88 ws-trans-layaway                   value 'L'.
           88 ws-trans-return                    value 'R'.

       01 ws-number-of-stores              pic 9 value 6.
       01 ws-store-index                   pic 9 value 0.

       01 ws-report-heading.
           05 filler                       pic x(40)
               value ' Counts And Control Totals -- Final Gr 8'.

       01 ws-totals.
           05 ws-tot-sl-rec                pic 9(4)
               value 0.
           05 ws-tot-sl-rec-amnt           pic 9(8)v99
               value 0.
           05 ws-tot-s-rec                 pic 9(4)
               value 0.
           05 ws-tot-s-rec-amnt            pic 9(8)v99
               value 0.
           05 ws-tot-l-rec                 pic 9(4)
               value 0.
           05 ws-tot-l-rec-amnt            pic 9(8)v99
               value 0.
           05 ws-tot-s-rec-ptc             pic 999v99
               value 0.
           05 ws-tot-l-rec-ptc             pic 999v99
               value 0.
           05 ws-tot-r-rec                 pic 9(4)
               value 0.
           05 ws-tot-r-rec-amnt            pic 9(8)v99
               value 0.
           05 ws-tot-store-tbl             occurs 6 times.
               10 ws-tot-store-sl-rec-amnt pic 9(8)v99.
               10 ws-tot-store-r-rec       pic 9(4).
               10 ws-tot-store-r-rec-amnt  pic 9(8)v99.

       01 ws-td-sl-records.
           05 filler                       pic x value space.
           05 filler                       pic x(27)
               value 'Total S&L Records:         '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-sl-rec                 pic Z,ZZ9.

       01 ws-td-sl-amount.
           05 filler                       pic x value space.
           05 filler                       pic x(18)
               value 'Total S&L Amount: '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-sl-rec-amnt            pic $$$,$$$,$$9.99.

       01 ws-td-s-records.
           05 filler                       pic x value space.
           05 filler                       pic x(27)
               value 'Total S Records:           '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-s-rec                  pic Z,ZZ9.

       01 ws-td-s-amount.
           05 filler                       pic x value space.
           05 filler                       pic x(18)
               value 'Total S Amount:   '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-s-rec-amnt             pic $$$,$$$,$$9.99.

       01 ws-td-l-records.
           05 filler                       pic x value space.
           05 filler                       pic x(27)
               value 'Total L Records:           '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-l-rec                  pic Z,ZZ9.

       01 ws-td-l-amount.
           05 filler                       pic x value space.
           05 filler                       pic x(18)
               value 'Total L Amount:   '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-l-rec-amnt             pic $$$,$$$,$$9.99.

       01 ws-td-s-percent.
           05 filler                       pic x value space.
           05 filler                       pic x(26)
               value 'Percent S:                '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-s-rec-pct              pic ZZ9.9.
           05 filler                       pic x
               value '%'.

       01 ws-td-l-percent.
           05 filler                       pic x value space.
           05 filler                       pic x(26)
               value 'Percent R:                '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-l-rec-pct              pic ZZ9.9.
           05 filler                       pic x
               value '%'.

       01 ws-td-r-records.
           05 filler                       pic x value space.
           05 filler                       pic x(27)
               value 'Total R Records:           '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-r-rec                  pic Z,ZZ9.

       01 ws-td-r-amount.
           05 filler                       pic x value space.
           05 filler                       pic x(18)
               value 'Total R Amount:   '.
      *              0---+5+---1---+5+---2---+5+---3
           05 ws-td-r-rec-amnt             pic $$$,$$$,$$9.99.

           05 filler                       pic x value space.

       01 ws-td-store-heading1.
           05 filler                       pic x value space.
           05 filler
               value 'Transaction Count And Returns Amount By Store'.

       01 ws-td-store-heading2.
           05 filler                       pic x value space.
           05 filler                       pic x(9)
               value 'Store #  '.
      *              0---+5+---1---+5+---2---+5+---3
           05 filler                       pic x(16)
               value '    S&L Trans.  '.
      *              0---+5+---1---+5+---2---+5+---3
           05 filler                       pic x(9)
               value 'Returns  '.
      *              0---+5+---1---+5+---2---+5+---3
           05 filler                       pic x(14)
               value ' Returns Amnt.'.
      *              0---+5+---1---+5+---2---+5+---3

       01 ws-td-store                      occurs 6 times.
           05 filler                       pic x(3)  value spaces.
           05 ws-td-store-num              pic 99.
           05 filler                       pic x(5) value spaces.
           05 ws-td-store-sl-rec-amnt      pic $$$,$$$,$$9.99.
           05 filler                       pic x(4) value spaces.
           05 ws-td-store-r-rec            pic Z,ZZ9.
           05 filler                       pic xx   value spaces.
           05 ws-td-store-r-rec-amnt       pic $$$,$$$,$$9.99.


       procedure division.
       000-main.
           open input input-file,
               output sales-layaways-file,
                      returns-file,
                      report-file.

           read input-file
               at end move 'y'             to ws-eof-flag.

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
           move in-trans-code              to ws-trans-code.
      *
      *    NOTE: Sets the ws-store-index var.
           perform 220-calc-store-index.

           if (ws-trans-sale)
               add 1                       to ws-tot-sl-rec
               add in-trans-amnt           to ws-tot-sl-rec-amnt

               add 1                       to ws-tot-s-rec
               add in-trans-amnt           to ws-tot-s-rec-amnt

               add in-trans-amnt
                to ws-tot-store-sl-rec-amnt(ws-store-index)
           else if (ws-trans-layaway)
               add 1                       to ws-tot-sl-rec
               add in-trans-amnt           to ws-tot-sl-rec-amnt

               add 1                       to ws-tot-l-rec
               add in-trans-amnt           to ws-tot-l-rec-amnt

               add in-trans-amnt
                to ws-tot-store-sl-rec-amnt(ws-store-index)
           else if (ws-trans-return)
               add 1                       to ws-tot-r-rec
               add in-trans-amnt           to ws-tot-r-rec-amnt

               add 1
                to ws-tot-store-r-rec(ws-store-index)
               add in-trans-amnt
                to ws-tot-store-r-rec-amnt(ws-store-index)
           end-if
           end-if
           end-if.

      *    Lastly, we print the record to the appropriate file.
           perform 280-print-record.

           read input-file
               at end move 'y'             to ws-eof-flag.


       220-calc-store-index.
           if (in-store-num = 1)
               move 1                      to ws-store-index
           else if (in-store-num = 2)
               move 2                      to ws-store-index
           else if (in-store-num = 3)
               move 3                      to ws-store-index
           else if (in-store-num = 4)
               move 4                      to ws-store-index
           else if (in-store-num = 5)
               move 5                      to ws-store-index
           else if (in-store-num = 12)
               move 6                      to ws-store-index
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if.


       280-print-record.
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
           move ws-tot-sl-rec              to ws-td-sl-rec.
           move ws-tot-sl-rec-amnt         to ws-td-sl-rec-amnt.
           move ws-tot-s-rec               to ws-td-s-rec.
           move ws-tot-s-rec-amnt          to ws-td-s-rec-amnt.
           move ws-tot-l-rec               to ws-td-l-rec.
           move ws-tot-l-rec-amnt          to ws-td-l-rec-amnt.
           move ws-tot-r-rec               to ws-td-r-rec.
           move ws-tot-r-rec-amnt          to ws-td-r-rec-amnt.
           move ws-tot-s-rec-ptc           to ws-td-s-rec-pct.
           move ws-tot-l-rec-ptc           to ws-td-l-rec-pct.
      *
      *    Now we move the store total table vals to the display var.
           perform 420-set-store-numbers.
           perform varying ws-store-index from 1 by 1
             until ws-store-index > ws-number-of-stores

               move ws-tot-store-sl-rec-amnt(ws-store-index)
                 to ws-td-store-sl-rec-amnt(ws-store-index)
               move ws-tot-store-r-rec(ws-store-index)
                 to ws-td-store-r-rec(ws-store-index)
               move ws-tot-store-r-rec-amnt(ws-store-index)
                 to ws-td-store-r-rec-amnt(ws-store-index)

           end-perform.

           write report-output             from ws-td-sl-records.
           write report-output             from ws-td-sl-amount.
           write report-output             from ws-td-s-records.
           write report-output             from ws-td-s-amount.
           write report-output             from ws-td-l-records.
           write report-output             from ws-td-l-amount.
           write report-output             from ws-td-r-records.
           write report-output             from ws-td-r-amount.
           write report-output             from ws-td-s-percent.
           write report-output             from ws-td-l-percent
               before advancing 2 lines.

           write report-output             from ws-td-store-heading1.
           write report-output             from ws-td-store-heading2.
           perform varying ws-store-index from 1 by 1
             until ws-store-index > ws-number-of-stores

               write report-output
                from ws-td-store(ws-store-index)

           end-perform.


       420-set-store-numbers.
           move 1                          to ws-td-store-num(1)
           move 2                          to ws-td-store-num(2)
           move 3                          to ws-td-store-num(3)
           move 4                          to ws-td-store-num(4)
           move 5                          to ws-td-store-num(5)
           move 12                         to ws-td-store-num(6)


       end program DataSplitCount.
