# frozen_string_literal: true

module RecordGenerator
  NAME_LENGTH = 20
  HOURS_LENGTH = 6
  RATE_LENGTH = 6

  def generate_id_numbers
    while id_numbers.uniq.length < employee_names.length do
      id_numbers << roll_employee_id
    end

    id_numbers.uniq!
  end

  def generate_employee_record(id, name)
    format('%s%s%s%s', id, name.ljust(NAME_LENGTH), roll_hours_worked.rjust(HOURS_LENGTH), roll_rate.rjust(RATE_LENGTH))
  end

  def generate_roster
    generate_id_numbers

    employee_names.each_with_index do |name, index|
      roster << generate_employee_record(id_numbers[index], name)
    end

    roster << "" # COBOL Expects a newline at the end of input files
  end
end
