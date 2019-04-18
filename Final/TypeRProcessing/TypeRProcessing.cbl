       identification division.
       program-id. TypeRProcessing.
       author. Ryan Sim.
       date-written. 2019-04-18.

       environment division.
       input-output section.
       file-control.
           select input-file
               assign to "data/rrecords.dat"
               organization is line sequential.
           select output-file
               assign to "data/report-r.out"
               organization is line sequential.

       configuration section.

       data division.
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

           fd output-file
               data record is print-line
               record contains 74 characters.

           01 print-line                   pic x(74).

       working-storage section.
       01 ws-report-header.
           05 filler                       pic x(19)
               value " Returns -- Group 8".
       01 ws-page-header-1.
           05 filler                       pic x(27)
               value "Trans  Transaction  Payment".
           05 filler                       pic x(27)
               value "  Store  Invoice        SKU".
           05 filler                       pic x(20)
               value "              Tax   ".
       01 ws-page-header-2.
           05 filler                       pic x(31)
               value " Code    Amount      Type      ".
           05 filler                       pic x(24)
               value "#    Number         Code".
           05 filler                       pic x(19)
               value "            Owing  ".

       01 ws-detail-line.
           05 filler                       pic xx
               value spaces.
           05 ws-dl-trans-code             pic x.
           05 filler                       pic x(5)
               value spaces.
           05 ws-dl-trans-amnt             pic $$$,$$9.99.
           05 filler                       pic x(4)
               value spaces.
           05 ws-dl-pay-type               pic xx.
           05 filler                       pic x(7)
               value spaces.
           05 ws-dl-store-num              pic 99.
           05 filler                       pic xx
               value spaces.
           05 ws-dl-invoice-num            pic x(9).
           05 filler                       pic xxx
               value spaces.
           05 ws-dl-sku-code               pic x(15).
           05 filler                       pic xx
               value spaces.
           05 ws-dl-tax-owing              pic $$$,$$9.99.

       01 ws-total-lines.
           05 ws-tl-records-line.
               10 filler                   pic x(22)
                   value "TOTAL RETURN RECORDS: ".
               10 ws-tl-records            pic zz9.
           05 ws-tl-trans-amount-line.
               10 filler                   pic x(35)
                   value "TOTAL TRANSACTION AMOUNT (RETURNS):".
               10 ws-tl-trans-amount       pic $$$,$$$,$$9.99.
           05 ws-tl-store-records-line.
               10 filler                   pic x(22)
                   value "TOTAL RETURNS - Store ".
               10 ws-tl-store-records-num  pic 99.
               10 filler                   pic xx
                   value ": ".
               10 ws-tl-store-records      pic zz9.
           05 ws-tl-store-amount-line.
               10 filler                   pic x(28)
                   value "TOTAL RETURN AMOUNT - Store ".
               10 ws-tl-store-amount-num   pic 99.
               10 filler                   pic xx
                   value ": ".
               10 ws-tl-store-amount       pic $$$,$$$,$$9.99.
           05 ws-tl-tax-owing-line.
               10 filler                   pic x(17)
                   value "TOTAL TAX OWING: ".
               10 ws-tl-tax-owing          pic $$$,$$$,$$9.99.

       01 ws-constants.
           05 ws-records-per-page-cnst     pic 99
               value 20.
           05 ws-tax-perc-cnst             pic 99
               value 13.

       01 ws-detail-calc.
           05 ws-dc-tax-owing              pic 9(5)v99.

       01 ws-totals-calc.
           05 ws-total-records             pic 999.
           05 ws-total-amount              pic 9(8)v99.
           05 ws-total-tax-owing           pic 9(8)v99.
           05 ws-store-returns-table.
               10 ws-stt-size              pic 99.
               10 ws-stt-store-num         pic 99
                   occurs 6 times.
               10 ws-stt-total-records     pic 999
                   occurs 6 times.
               10 ws-stt-total-amount      pic 9(8)v99
                   occurs 6 times.

       01 ws-eof-flag                      pic x
           value 'n'.

       01 ws-index                   pic 99
           value zero.

       01 ws-page-records                  pic 99.

       procedure division.
       000-Main.
           open input  input-file.
           open output output-file.

           read input-file
               at end move 'y'             to ws-eof-flag.

      * Initialize totals
           move zeroes                     to ws-totals-calc.

           perform 100-print-report-header.

           perform 110-initialize-stores-table.

           perform 200-process-page
               until ws-eof-flag = 'y'.

           close input-file.

           perform 300-print-totals.

           goback.

       100-print-report-header.
           write print-line                from ws-report-header.
           write print-line                from spaces.

       110-initialize-stores-table.
           initialize ws-store-returns-table.
           move 6                          to ws-stt-size.

           move 01                         to ws-stt-store-num(1).
           move 02                         to ws-stt-store-num(2).
           move 03                         to ws-stt-store-num(3).
           move 04                         to ws-stt-store-num(4).
           move 05                         to ws-stt-store-num(5).
           move 12                         to ws-stt-store-num(6).

       200-process-page.
      * Initialize record count.
           move zero                       to ws-page-records.

      * Write page headers
           write print-line                from ws-page-header-1.
           write print-line                from ws-page-header-2.

           perform 210-process-record
               until ws-page-records >= ws-records-per-page-cnst
               or ws-eof-flag = "y".

      * Page break
           write print-line from spaces
               after page.

       210-process-record.
           move zeroes                     to ws-detail-calc.
           add 1                           to ws-page-records.

           move in-trans-code              to ws-dl-trans-code.
           move in-trans-amnt              to ws-dl-trans-amnt.
           move in-pay-type                to ws-dl-pay-type.
           move in-store-num               to ws-dl-store-num.
           move in-invoice-num             to ws-dl-invoice-num.
           move in-sku-code                to ws-dl-sku-code.

           perform 220-calculate-tax-owing.

           move ws-dc-tax-owing            to ws-dl-tax-owing.

           perform 290-accumulate-totals.

           write print-line                from ws-detail-line.

           read input-file
               at end move 'y'             to ws-eof-flag.

       220-calculate-tax-owing.
           compute ws-dc-tax-owing rounded =
             (in-trans-amnt / 100 * ws-tax-perc-cnst).

       290-accumulate-totals.
           add 1                           to ws-total-records.
           add in-trans-amnt               to ws-total-amount.

           perform 291-accumulate-store-total.

           add ws-dc-tax-owing             to ws-total-tax-owing.

       291-accumulate-store-total.
           move zero                       to ws-index.
           perform varying ws-index
             from 1 by 1
             until ws-index > ws-stt-size
               if (ws-stt-store-num(ws-index) equals in-store-num)
                   add 1
                     to ws-stt-total-records(ws-index)
                   add in-trans-amnt to ws-stt-total-amount(ws-index)
               end-if
           end-perform.

       300-print-totals.
           move ws-total-records           to ws-tl-records.
           move ws-total-amount            to ws-tl-trans-amount.

           perform varying ws-index
             from 1 by 1
             until ws-index > ws-stt-size
               move ws-stt-store-num(ws-index)
                 to ws-tl-store-records-num, ws-tl-store-amount-num
               move ws-stt-total-records(ws-index)
                 to ws-tl-store-records
               move ws-stt-total-amount(ws-index)
                 to ws-tl-store-amount
               write print-line            from ws-tl-store-records-line
                   after advancing 1 line
               write print-line            from ws-tl-store-amount-line
           end-perform.

           write print-line                from ws-tl-records-line
               after advancing 1 line.
           write print-line                from ws-tl-trans-amount-line.

           move ws-total-tax-owing         to ws-tl-tax-owing.
           
           write print-line                from ws-tl-tax-owing-line
               after advancing 1 line.

       end program TypeRProcessing.