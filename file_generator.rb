# frozen_string_literal: true

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
  'Tina Evans',
]

def roll_employee_id
  '%05i' % rand(0..99999)
end

def generate_line(name)
  format('%s%s%s%s', roll_employee_id, name.ljust(20), roll_hours_worked.rjust(6), roll_rate.rjust(6))
end

def roll_hours_worked
  '%02i00' % rand(32..40)
end

def roll_rate
  '%02i00' % rand(17..25)
end

report = []

NAME_LIST.each do |name|
  report << generate_line(name)
end

IO.write 'employee_data.txt', report.join("\n")
