IDENTIFICATION DIVISION.
PROGRAM-ID. Payroll.
AUTHOR. LDA.
DATE-WRITTEN. 2025-01-09.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT InputFile ASSIGN TO "lib/employee_data.txt"
			ORGANIZATION IS LINE SEQUENTIAL.
	SELECT OutputFile ASSIGN TO "lib/payroll_report.txt"
			ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD InputFile.
01 InputRecord.
	02 employeeID 				PIC 9(5) VALUE ZEROS.
	02 employeeName				PIC X(20) VALUE SPACES.
	02 hoursWorked				PIC 9(4)V99 VALUE ZEROS.
	02 rate 					PIC 9(4)V99 VALUE ZEROS.
	02 hoursWorkedFormatted		PIC Z(4).99 VALUE ZEROS.
	02 rateFormatted 			PIC Z(4).99 VALUE ZEROS.

FD OutputFile.
01 OutputRecord PIC X(50) VALUE SPACES.

WORKING-STORAGE SECTION.
01 TotalEmployees.
	02 TotalEmployeesRaw		PIC 9(4) VALUE ZEROS.
	02 TotalEmployeesFormatted	PIC Z,ZZZ VALUE ZEROS. 

01 TotalGrossPay.
	02 TotalGrossPayRaw			PIC 9(8)V99 VALUE ZEROS.
	02 TotalGrossPayFormatted	PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS. 

01 AverageGrossPay.
	02 AverageGrossPayRaw		PIC 9(8)V99 VALUE ZEROS. 
	02 AverageGrossPayFormatted	PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS. 

01 GrossPay. 		
	02 GrossPayRaw			PIC 9(4)V99 VALUE ZEROS.
	02 GrossPayFormatted	PIC Z,ZZZ.99 VALUE ZEROS. 

01 Header PIC X(50) VALUE "NAME                   ID  HOURS   RATE GROSS-PAY".

01 EndOfFileFlag PIC 9 VALUE ZERO.
	88 NotEndOfFile VALUE 0.
	88 EndOfFile VALUE 1.

PROCEDURE DIVISION.
Main-Logic.
	OPEN INPUT InputFile
		 OUTPUT OutputFile

	PERFORM Write-Header

	READ InputFile INTO InputRecord
		AT END SET EndOfFile TO TRUE
	END-READ

	PERFORM UNTIL EndOfFile
		PERFORM Calculate-Employee-Gross-Pay
		PERFORM Caclulate-Employee-and-Gross-Pay-Totals

		PERFORM Prepare-Output-Record
		WRITE OutputRecord

		READ InputFile INTO InputRecord
			AT END SET EndOfFile TO TRUE
		END-READ
	END-PERFORM

	PERFORM Calculate-Average-Gross-Pay
	PERFORM Write-Footer

	CLOSE InputFile, OutputFile
STOP RUN.

CALCULATION SECTION.
Calculate-Employee-Gross-Pay.
	COMPUTE GrossPayRaw = hoursWorked * rate
	MOVE GrossPayRaw TO GrossPayFormatted.

Caclulate-Employee-and-Gross-Pay-Totals.
	ADD 1 TO TotalEmployeesRaw
	ADD GrossPayRaw TO TotalGrossPayRaw.

Calculate-Average-Gross-Pay.
	IF TotalEmployees > 0
		COMPUTE AverageGrossPayRaw = TotalGrossPayRaw / TotalEmployeesRaw
	ELSE
		MOVE 0 TO AverageGrossPayRaw
	END-IF.

WRITING SECTION.
Write-Header.
	MOVE "PAYROLL REPORT" to OutputRecord
	WRITE OutputRecord

	MOVE Header TO OutputRecord
	WRITE OutputRecord
	PERFORM Clear-Output.

Write-Footer.
	MOVE TotalGrossPayRaw TO TotalGrossPayFormatted
	MOVE TotalEmployeesRaw TO TotalEmployeesFormatted
	MOVE AverageGrossPayRaw TO AverageGrossPayFormatted

	PERFORM Write-Newline

	STRING "TOTAL GROSS PAY: " DELIMITED BY SIZE
	FUNCTION TRIM (TotalGrossPayFormatted) DELIMITED BY SIZE
	INTO OutputRecord
	WRITE OutputRecord

	STRING "TOTAL EMPLOYEES: " DELIMITED BY SIZE
	FUNCTION TRIM (TotalEmployeesFormatted) DELIMITED BY SIZE 
	INTO OutputRecord
	WRITE OutputRecord

	STRING "AVERAGE GROSS PAY: " DELIMITED BY SIZE
	FUNCTION TRIM (AverageGrossPayFormatted) DELIMITED BY SIZE
	INTO OutputRecord
	WRITE OutputRecord.

Prepare-Output-Record.
	MOVE hoursWorked TO hoursWorkedFormatted
	MOVE rate TO rateFormatted

	STRING employeeName DELIMITED BY SIZE
	employeeID DELIMITED BY SIZE
	hoursWorkedFormatted DELIMITED BY SIZE
	rateFormatted DELIMITED BY SIZE
	SPACE SPACE GrossPayFormatted DELIMITED BY SIZE
	INTO OutputRecord.

Write-Newline.
	PERFORM Clear-Output
	WRITE OutputRecord.

Clear-Output.
	MOVE SPACES TO OutputRecord.
