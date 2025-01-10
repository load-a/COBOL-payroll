# frozen_string_literal: true

module Roller
  ID_NUMBER_RANGE = (0..99999)
  FULL_TIME_HOUR_RANGE = (32..40)
  PAY_RATE_RANGE = (17..25)

  def roll_employee_id
    '%05i' % rand(ID_NUMBER_RANGE)
  end

  def roll_hours_worked
    # Item is expected to have trailing zeros
    '%02i00' % rand(FULL_TIME_HOUR_RANGE)
  end

  def roll_rate
    # Item is expected to have trailing zeros
    '%02i00' % rand(PAY_RATE_RANGE)
  end
end
