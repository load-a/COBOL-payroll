# frozen_string_literal: true

require_relative '../generator_modules/roller'
require_relative '../generator_modules/record_generator'

class EmployeeDataGenerator
  include Roller
  include RecordGenerator

  attr_accessor :id_numbers, :employee_names, :roster

  def initialize(employee_names)
    self.employee_names = employee_names
    self.id_numbers = []
    self.roster = []
  end

  def write_employee_file(filename)
    generate_roster
    IO.write filename, roster.join("\n")
  end
end

NAME_LIST = [
  'Alice Johnson',
  'Bob Smith',
  'Carla Gomez',
  'David Brown',
  'Emily Davis',
  'Frank Harris',
  'Grace Miller',
  'Henry Wilson',
  'Isabella Moore',
  'Jack Taylor',
  'Karen Clark',
  'Liam Hall',
  'Megan Adams',
  'Nathan Reed',
  'Olivia Lewis',
  'Peter White',
  'Quinn Thompson',
  'Rachel King',
  'Samuel Wright',
  'Tina Evans'
].map(&:split).map(&:reverse).map { |name| name.join(", ")}.sort
# Sure, I could've just used a sorted list from the start
# but where's the fun in that?

raise "No File Detected. A file path is needed to write the report." if ARGV.empty?

EmployeeDataGenerator.new(NAME_LIST).write_employee_file(ARGV.first)
