       identification division.
       program-id. SLProcessing.
       author. Ryan Sim.
       date-written. 2019-04-18.

       environment division.
       input-output section.
       file-control.
           select input-file
               assign to "data/slrecords.dat"
               organization is line sequential.
           select output-file
               assign to "data/report-sl.out"
               organization is line sequential.

       configuration section.

       data division.
           fd input-file
               data record is input-record
               record contains 36 characters.

           01 input-record.
               05 in-trans-code            pic x.
                   88 in-trans-code-88-sale
                       value "S".
                   88 in-trans-code-88-layaway
                       value "L".
               05 in-trans-amnt            pic 9(5)v99.
               05 in-pay-type              pic xx.
                   88 in-pay-type-88-cash
                       value "CA".
                   88 in-pay-type-88-credit
                       value "CR".
                   88 in-pay-type-88-debit
                       value "DB".
               05 in-store-num             pic 99.
               05 in-invoice-num           pic x(9).
               05 in-sku-code              pic x(15).

           fd output-file
               data record is print-line
               record contains 74 characters.

           01 print-line                   pic x(74).

       working-storage section.
       01 ws-report-header.
           05 filler                       pic x(28)
               value " Sales & Layaways -- Group 8".
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
               10 filler                   pic x(19)
                   value "TOTAL S&L RECORDS: ".
               10 ws-tl-records            pic zz9.
           05 ws-tl-trans-amount-line.
               10 filler                   pic x(30)
                   value "TOTAL S&L TRANSACTION AMOUNT: ".
               10 ws-tl-trans-amount       pic $$$,$$$,$$9.99.
           05 ws-tl-S-records-line.
               10 filler                   pic x(20)
                   value "TOTAL SALE RECORDS: ".
               10 ws-tl-S-records            pic zz9.
           05 ws-tl-S-trans-amount-line.
               10 filler                   pic x(34)
                   value "TOTAL TRANSACTION AMOUNT (SALES): ".
               10 ws-tl-S-trans-amount       pic $$$,$$$,$$9.99.
           05 ws-tl-L-records-line.
               10 filler                   pic x(23)
                   value "TOTAL LAYAWAY RECORDS: ".
               10 ws-tl-L-records            pic zz9.
           05 ws-tl-L-trans-amount-line.
               10 filler                   pic x(37)
                   value "TOTAL TRANSACTION AMOUNT (LAYAWAYS): ".
               10 ws-tl-L-trans-amount       pic $$$,$$$,$$9.99.
           05 ws-tl-CA-payments-line.
               10 filler                   pic x(17)
                   value "CASH PAYMENTS:   ".
               10 ws-tl-CA-payments        pic zz9.
               10 filler                   pic x(3)
                   value " - ".
               10 ws-tl-CA-percent         pic zz9.99.
               10 filler                   pic x
                   value "%".
           05 ws-tl-CR-payments-line.
               10 filler                   pic x(17)
                   value "CREDIT PAYMENTS: ".
               10 ws-tl-CR-payments        pic zz9.
               10 filler                   pic x(3)
                   value " - ".
               10 ws-tl-CR-percent         pic zz9.99.
               10 filler                   pic x
                   value "%".
           05 ws-tl-DB-payments-line.
               10 filler                   pic x(17)
                   value "DEBIT PAYMENTS:  ".
               10 ws-tl-DB-payments        pic zz9.
               10 filler                   pic x(3)
                   value " - ".
               10 ws-tl-DB-percent         pic zz9.99.
               10 filler                   pic x
                   value "%".
           05 ws-tl-tax-owing-line.
               10 filler                   pic x(17)
                   value "TOTAL TAX OWING: ".
               10 ws-tl-tax-owing          pic $$$,$$$,$$9.99.
           05 ws-tl-best-store-line.
               10 filler                   pic x(34)
                   value "HIGHEST TRANSACTION AMOUNT: Store ".
               10 ws-tl-best-store-num     pic 99.
           05 ws-tl-worst-store-line.
               10 filler                   pic x(34)
                   value "LOWEST TRANSACTION AMOUNT:  Store ".
               10 ws-tl-worst-store-num    pic 99.

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
           05 ws-total-S-records           pic 999.
           05 ws-total-S-amount            pic 9(8)v99.
           05 ws-total-L-records           pic 999.
           05 ws-total-L-amount            pic 9(8)v99.
           05 ws-total-CA                  pic 999.
           05 ws-perc-CA                   pic 999v9.
           05 ws-total-CR                  pic 999.
           05 ws-perc-CR                   pic 999v9.
           05 ws-total-DB                  pic 999.
           05 ws-perc-DB                   pic 999v9.
           05 ws-total-tax-owing           pic 9(8)v99.
           05 ws-store-totals-table.
               10 ws-stt-size              pic 99.
               10 ws-stt-store-num         pic 99
                   occurs 6 times.
               10 ws-stt-total-trans       pic 9(8)v99
                   occurs 6 times.
           05 ws-highest-trans.
               10 ws-highest-trans-amt     pic 9(5)v99
                   value zero.
               10 ws-highest-store-num     pic 99.
           05 ws-lowest-trans.
               10 ws-lowest-trans-amt      pic 9(5)v99
                   value zero.
               10 ws-lowest-store-num      pic 99.

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
           initialize ws-store-totals-table.
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

           if in-trans-code-88-sale then
               add 1                       to ws-total-S-records
               add in-trans-amnt           to ws-total-S-amount
           else if in-trans-code-88-layaway then
               add 1                       to ws-total-L-records
               add in-trans-amnt           to ws-total-L-amount
           end-if
           end-if.

           if in-pay-type-88-cash then
               add 1                       to ws-total-CA
           else if in-pay-type-88-credit then
               add 1                       to ws-total-CR
           else if in-pay-type-88-debit then
               add 1                       to ws-total-DB
           end-if
           end-if
           end-if.

           perform 291-accumulate-store-total.

           add ws-dc-tax-owing             to ws-total-tax-owing.

       291-accumulate-store-total.
           move zero                       to ws-index.
           perform varying ws-index
             from 1 by 1
             until ws-index > ws-stt-size
               if (ws-stt-store-num(ws-index) equals in-store-num)
                   add in-trans-amnt to ws-stt-total-trans(ws-index)
               end-if
           end-perform.

       300-print-totals.
           move ws-total-records           to ws-tl-records.
           move ws-total-amount            to ws-tl-trans-amount.

           write print-line                from ws-tl-records-line
               after advancing 1 line.
           write print-line                from ws-tl-trans-amount-line.

           move ws-total-S-records         to ws-tl-S-records.
           move ws-total-S-amount          to ws-tl-S-trans-amount.
           move ws-total-L-records         to ws-tl-L-records.
           move ws-total-L-amount          to ws-tl-L-trans-amount.

           write print-line                from ws-tl-S-records-line
               after advancing 1 line.
           write print-line
               from ws-tl-S-trans-amount-line.
           write print-line                from ws-tl-L-records-line.
           write print-line
               from ws-tl-L-trans-amount-line.

           perform 310-calculate-payment-percentages.
           move ws-total-CA                to ws-tl-CA-payments.
           move ws-perc-CA                 to ws-tl-CA-percent.
           move ws-total-CR                to ws-tl-CR-payments.
           move ws-perc-CR                 to ws-tl-CR-percent.
           move ws-total-DB                to ws-tl-DB-payments.
           move ws-perc-DB                 to ws-tl-DB-percent.

           write print-line                from ws-tl-CA-payments-line
               after advancing 1 line.
           write print-line                from ws-tl-CR-payments-line.
           write print-line                from ws-tl-DB-payments-line.

           move ws-total-tax-owing         to ws-tl-tax-owing.
           
           write print-line                from ws-tl-tax-owing-line
               after advancing 1 line.

           perform 320-determine-highest-lowest-stores.
           move ws-highest-store-num       to ws-tl-best-store-num.
           move ws-lowest-store-num        to ws-tl-worst-store-num.

           write print-line                from ws-tl-best-store-line
               after advancing 1 line.
           write print-line                from ws-tl-worst-store-line.

       310-calculate-payment-percentages.
           compute ws-perc-CA rounded =
             (ws-total-CA / ws-total-records * 100).
           compute ws-perc-CR rounded =
             (ws-total-CR / ws-total-records * 100).
           compute ws-perc-DB rounded =
             (ws-total-DB / ws-total-records * 100).

       320-determine-highest-lowest-stores.
           move zero                       to ws-index.
           move ws-stt-total-trans(1)      to ws-lowest-trans-amt.
           perform varying ws-index
             from 1 by 1
             until ws-index > ws-stt-size
               if (ws-stt-total-trans(ws-index) < ws-lowest-trans-amt)
                   move ws-stt-total-trans(ws-index)
                     to ws-lowest-trans-amt
                   move ws-stt-store-num(ws-index)
                     to ws-lowest-store-num
               else
               if (ws-stt-total-trans(ws-index) > ws-highest-trans-amt)
                   move ws-stt-total-trans(ws-index)
                     to ws-highest-trans-amt
                   move ws-stt-store-num(ws-index)
                     to ws-highest-store-num
               end-if
               end-if
           end-perform.

       end program SLProcessing.