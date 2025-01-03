IDENTIFICATION DIVISION.
PROGRAM-ID. Payroll.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT InputFile ASSIGN TO "employee_data.txt"
			ORGANIZATION IS LINE SEQUENTIAL.
	SELECT OutputFile ASSIGN TO "payroll_report.txt"
			ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD InputFile.
01 InputRecord.
	05 employeeID 	PIC 9(5).
	05 employeeName	PIC X(20).
	05 hoursWorked	PIC 9(4)V99.
	05 rate 		PIC 9(4)V99.

FD OutputFile.
01 OutputRecord PIC X(50).

WORKING-STORAGE SECTION.
01 TotalEmployees.
	05 TotalEmployeesRaw		PIC 9(4).
	05 TotalEmployeesFormatted	PIC Z(4). 
01 TotalGrossPay.
	05 TotalGrossPayRaw			PIC 9(8)V99.
	05 TotalGrossPayFormatted	PIC Z(8).99. 
01 AverageGrossPay.
	05 AverageGrossPayRaw		PIC 9(8)V99. 
	05 AverageGrossPayFormatted	PIC Z(8).99. 
01 grossPay. 		
	05 grossPayRaw			PIC 9(4)V99.
	05 grossPayFormatted	PIC Z(4).99.
01 FileStatus 			PIC 9 VALUE 0.
	88 EndOfFile		VALUE 1.
01 Header 	PIC X(50) VALUE "NAME                ID   HOURS RATE  GROSS-PAY".

PROCEDURE DIVISION.
Main-Logic.
	OPEN INPUT InputFile
		 OUTPUT OutputFile.

	PERFORM Write-Header.

	PERFORM UNTIL EndOfFile
		READ InputFile INTO InputRecord
			AT END
				SET EndOfFile TO TRUE
				EXIT PERFORM
			NOT AT END
				PERFORM Calculate-Gross-Pay
				PERFORM Record-Totals
				PERFORM Generate-Output-Line

				*> It reads an extra blank line (despite one not existing in the file) so I've adjusted for it here.
				IF employeeName = SPACES
					PERFORM Clear-OuputRecord
					SUBTRACT 1 FROM TotalEmployeesRaw
				END-IF

				WRITE OutputRecord
				PERFORM Clear-OuputRecord
		END-READ
	END-PERFORM.

	PERFORM Calculate-Average-Gross-Pay.
	PERFORM Write-Footer.

	CLOSE InputFile
		  OutputFile.
STOP RUN.

Calculate-Gross-Pay.
	COMPUTE grossPayRaw = hoursWorked * rate.
	MOVE grossPayRaw TO grossPayFormatted.

Generate-Output-Line.
	STRING employeeName DELIMITED BY SIZE
	employeeID DELIMITED BY SIZE
	hoursWorked DELIMITED BY SIZE
	rate DELIMITED BY SIZE
	grossPayFormatted DELIMITED BY SIZE
	INTO OutputRecord.

Record-Totals.
	ADD 1 TO TotalEmployeesRaw.
	ADD grossPayRaw TO TotalGrossPayRaw.

Calculate-Average-Gross-Pay.
	IF TotalEmployees > 0
		COMPUTE AverageGrossPayRaw = TotalGrossPayRaw / TotalEmployeesRaw
	ELSE
		MOVE 0 TO AverageGrossPayRaw.

Write-Header.
	MOVE "PAYROLL REPORT" to OutputRecord.
	WRITE OutputRecord.

	PERFORM Newline.

	MOVE Header TO OutputRecord.
	WRITE OutputRecord.
	PERFORM Newline.

Write-Footer.
	MOVE TotalGrossPayRaw TO TotalGrossPayFormatted.
	MOVE TotalEmployeesRaw TO TotalEmployeesFormatted.
	MOVE AverageGrossPayRaw TO AverageGrossPayFormatted.

	STRING "TOTAL GROSS PAY: " DELIMITED BY SIZE
	TotalGrossPayFormatted DELIMITED BY SIZE
	INTO OutputRecord.
	WRITE OutputRecord.
	PERFORM Clear-OuputRecord.

	STRING "TOTAL EMPLOYEES: " DELIMITED BY SIZE
	TotalEmployeesFormatted DELIMITED BY SIZE 
	INTO OutputRecord.
	WRITE OutputRecord.
	PERFORM Clear-OuputRecord.

	STRING "AVERAGE GROSS PAY: " DELIMITED BY SIZE
	AverageGrossPayFormatted DELIMITED BY SIZE 
	INTO OutputRecord.
	WRITE OutputRecord.
	PERFORM Clear-OuputRecord.

Clear-OuputRecord.
	MOVE SPACES TO OutputRecord.

Newline.
	PERFORM Clear-OuputRecord.
	WRITE OutputRecord.
