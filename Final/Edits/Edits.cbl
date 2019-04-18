       identification division.
       program-id. Edits.
       author. Connor Trentadue.
       
       environment division.
       input-output section.
       file-control.
           select input-file
               assign to '../build/data/project3.dat'
               organization is line sequential.
               
           select report-file
               assign to '../build/data/valid.out'
               organization is line sequential.
               
           select invalid-file
               assign to '../build/data/invalid.out'
               organization is line sequential.
               
           select errors-file
               assign to '../build/data/errors.out'
               organization is line sequential.

       data division.
       file section.
       
           fd input-file
               data record is edit-rec
               record contains 36 characters.
           
           01 edit-rec.
               05 transaction-code             pic x.
                   88 ws-valid-trans-code
                       values 'S', 'R', 'L'.
               05 transaction-amount           pic 9(5)v99.
               05 payment-type                 pic xx.
                   88 ws-valid-pay-type
                       values 'CA', 'CR', 'DB'.
               05 store-number                 pic xx.
                   88 ws-valid-store-num
                       values '01', '02', '03', '04', '05', '12'.
               05 invoice-number               pic x(9).
               05 invoice-number-r redefines
                   invoice-number.
                   10 inv-num-1                pic x.
                       88 ws-valid-inv-code
                           value 'A', 'B', 'C', 'D', 'E'.
                   10 inv-num-2                pic x.
                       88 ws-valid-inv-code-2
                           value 'A', 'B', 'C', 'D', 'E'.
                   10 inv-dash                 pic x.
                   10 inv-value                pic 9(6).
               05 sku-code                     pic x(15).
               
           fd report-file
               data record is report-line
               record contains 132 characters.
               
           01 report-line                      pic x(132).
           
           fd invalid-file
               data record is invalid-line
               record contains 132 characters.
               
           01 invalid-line                     pic x(132).
           
           fd errors-file
               data record is errors-line
               record contains 160 characters.
               
           01 errors-line                      pic x(160).
           
           
       working-storage section.
       
      *01 ws-constants.
       
       01 ws-eof-flag                          pic x   value 'n'.
           88 ws-end-of-file                           value 'y'.
           
       01 ws-detail-line.
           05 ws-detail-code                   pic x.
           05 ws-detail-amount                 pic 9(5)v99.
           05 ws-detail-pay-type               pic xx.
           05 ws-detail-store-num              pic xx.
           05 ws-detail-invoice-num            pic x(9).
           05 ws-detail-sku                    pic x(15).
           
       01 ws-invalid-line.
           05 ws-invalid-code                   pic x.
           05 ws-invalid-amount                 pic 9(5)v99.
           05 ws-invalid-pay-type               pic xx.
           05 ws-invalid-store-num              pic xx.
           05 ws-invalid-invoice-num            pic x(9).
           05 ws-invalid-sku                    pic x(15).
           
       01 ws-errors-line1.
           05 filler                           pic x(6)
               value "RECORD".
           05 filler                           pic x(3)
               value spaces.
           05 filler                           pic x(6)
               value "ERRORS".
               
       01 ws-errors-line2.
           05 filler                           pic x(3)
               value spaces.
           05 filler                           pic x(1)
               value "#".
       
       01 ws-errors-line3.
           05 filler                           pic x(2)
               value spaces.
           05 ws-error-num                     pic Z9
               value 0.
           05 filler                           pic x(5)
               value spaces.
           05 ws-error-arr                     pic x(35).
           05 ws-error-r redefines
               ws-error-arr                    occurs 10 times.
               10 ws-error-t                   pic x(32).
               10 filler                       pic x(3).
               
       01 ws-error-total-line-1.
           05 filler                           pic x(21)
               value "Total Valid Records: ".
           05 filler                           pic x(5)
               value spaces.
           05 ws-total-valid                   pic Z99
               value 0.
               
       01 ws-error-total-line-2.
           05 filler                           pic x(23)
               value "Total Invalid Records: ".
           05 filler                           pic x(3)
               value spaces.
           05 ws-total-invalid                 pic Z99
               value 0.
               
           
       01 ws-constants.
           05 ws-inv-max                       pic 9(6)
               value 900000.
           05 ws-inv-min                       pic 9(6)
               value 100000.
           05 ws-errors                        pic 99
               value 0.
           05 ws-total-errors                  pic 99
               value 0.
           05 ws-invalid-count                 pic 999
               value 0.
           05 ws-valid-count                   pic 999
               value 0.
           05 ws-transaction-error             pic x(24)
               value "INVALID TRANSACTION CODE".
           05 ws-empty-transaction             pic x(22)
               value "EMPTY TRANSACTION CODE".
           05 ws-invalid-trans-amount          pic x(30)
               value "TRANSACTION AMOUNT NOT NUMERIC".
           05 ws-pay-type-error                pic x(20)
               value "INVALID PAYMENT TYPE".
           05 ws-store-error                   pic x(20)
               value "INVALID STORE NUMBER".
           05 ws-invoice-code-error-1          pic x(30)
               value "INVOICE CODE IS NOT ALPHABETIC".
           05 ws-invoice-code-error-2          pic x(20)
               value "INVALID INVOICE CODE".
           05 ws-invoice-code-error-3          pic x(32)
               value "INVOICE CODE CANNOT BE REPEATING".
           05 ws-invoice-value-error-1         pic x(28)
               value "INVOICE VALUE IS NOT NUMERIC".
           05 ws-invoice-value-error-2         pic x(26)
               value "INVOICE VALUE NOT IN RANGE".
           05 ws-sku-error                     pic x(26)
               value "SKU CODE IS NOT ALPHABETIC".
           

       procedure division.
       
       000-main.
           open input input-file,
               output report-file, 
               invalid-file, errors-file.
               
           
           perform 500-print-error-headers.
           
           perform 100-read-input-file.
           
           perform 200-process-records
               until ws-end-of-file.
           
           perform 600-print-totals.
           
           close input-file,
                   report-file, 
                   invalid-file,
                   errors-file.

           stop run.
           
           
       100-read-input-file.
       
           read input-file
               at end move 'y'                 to ws-eof-flag.
               
       200-process-records.
       
           perform 300-validate-data.
           
           if not ws-detail-line = spaces then
               write report-line from ws-detail-line
           end-if.
           
           perform 100-read-input-file.
           
       300-validate-data.
           
           move spaces                         to ws-detail-line.
           move spaces                         to ws-invalid-line.
           move spaces                         to ws-errors-line3.
           move 0                              to ws-errors.
           
           if not ws-valid-trans-code then
               add 1 to ws-errors
               move ws-transaction-error       to ws-error-r(ws-errors)
           end-if.
           if transaction-code = spaces then
               add 1 to ws-errors
               move ws-empty-transaction       to ws-error-r(ws-errors)
           end-if.
           if not transaction-amount is numeric then
               add 1 to ws-errors
               move ws-invalid-trans-amount    to ws-error-r(ws-errors)
           end-if.
           if not ws-valid-pay-type then
               add 1 to ws-errors
               move ws-pay-type-error          to ws-error-r(ws-errors)
           end-if.
           if not ws-valid-store-num then
               add 1 to ws-errors
               move ws-store-error             to ws-error-r(ws-errors)
           end-if.
           if inv-num-1 = inv-num-2 then
               add 1 to ws-errors
               move ws-invoice-code-error-3    to ws-error-r(ws-errors)
           end-if.
           if not inv-value is numeric then
               add 1 to ws-errors
               move ws-invoice-value-error-1   to ws-error-r(ws-errors)
           end-if.
           if not ws-valid-inv-code then
               add 1 to ws-errors
               move ws-invoice-code-error-2    to ws-error-r(ws-errors)
           end-if.
           if not ws-valid-inv-code-2 then
               add 1 to ws-errors
               move ws-invoice-code-error-2    to ws-error-r(ws-errors)
           end-if.
           if not inv-value < ws-inv-max then
               add 1 to ws-errors
               move ws-invoice-value-error-2   to ws-error-r(ws-errors)
           end-if.
           if not inv-value > ws-inv-min then
               add 1 to ws-errors
               move ws-invoice-value-error-2   to ws-error-r(ws-errors)
           end-if.
           if sku-code = spaces then
               add 1 to ws-errors
               move ws-sku-error               to ws-error-r(ws-errors)
           end-if.
           
           if ws-errors = 0 then
               add 1                           to ws-valid-count
               move transaction-code           to ws-detail-code
               move transaction-amount         to ws-detail-amount
               move payment-type               to ws-detail-pay-type
               move store-number               to ws-detail-store-num
               move invoice-number             to ws-detail-invoice-num
               move sku-code                   to ws-detail-sku
           else
               add 1                           to ws-invalid-count
               move transaction-code           to ws-invalid-code
               move transaction-amount         to ws-invalid-amount
               move payment-type               to ws-invalid-pay-type
               move store-number               to ws-invalid-store-num
               move invoice-number             to ws-invalid-invoice-num
               move sku-code                   to ws-invalid-sku
               
               perform 400-write-errors
           end-if.

       400-write-errors.
           if not ws-invalid-line = spaces then
               write invalid-line from ws-invalid-line
           end-if.
           
           move ws-invalid-count               to ws-error-num.
           
           write errors-line from ws-errors-line3
               after advancing 1 line.
           
       500-print-error-headers.
           write errors-line from ws-errors-line1
               after advancing 1 line.
           write errors-line from ws-errors-line2
               after advancing 1 line.
               
       600-print-totals.
       
           move ws-invalid-count               to ws-total-invalid.
           move ws-valid-count                 to ws-total-valid.
       
           write errors-line from ws-error-total-line-1
               after advancing 2 line.
           write errors-line from ws-error-total-line-2
               after advancing 1 line.
           
       end program Edits.