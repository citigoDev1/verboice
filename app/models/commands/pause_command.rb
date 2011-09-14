class PauseCommand < Command
  param :length, :integer, :default => 1, :ui_length => 3

  def initialize(length = 1)
    @length = length
  end

  def run(session)
    EM.fiber_sleep @length
  end
end