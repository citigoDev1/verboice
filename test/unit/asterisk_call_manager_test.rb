require 'test_helper'

class AsteriskCallManagerTest < ActiveSupport::TestCase
  setup do
    @call_manager = Asterisk::CallManager.new 1
    @seq = sequence('seq')
  end

  [
    [:session_id, 'arg_1'],
    [:caller_id, 'callerid']
  ].each do |method, key|
    test "#{method}" do
      @call_manager.expects(:[]).with(key).returns :id
      assert_equal :id, @call_manager.send(method)
    end
  end

  test "channel_id" do
    @call_manager.expects(:[]).with('extension').returns '123'
    assert_equal 123, @call_manager.channel_id
  end

  test 'answer' do
    @call_manager.expects(:send_command).with('ANSWER')
    @call_manager.answer
  end

  test 'hangup' do
    @call_manager.expects(:send_command).with('HANGUP')
    @call_manager.expects :close_connection
    @call_manager.hangup
  end

  context "play" do
    setup do
      @path = @call_manager.sound_path_for 'something'
    end

    should "play" do
      @call_manager.expects(:stream_file).with('verboice/something', nil).returns(line '1'.ord.to_s)
      value = @call_manager.play @path
      assert_equal '1', value
    end

    should "play with escape digits" do
      @call_manager.expects(:stream_file).with('verboice/something', '123').returns(line '0')
      value = @call_manager.play @path, '123'
      assert_nil value
    end

    should "play throws exception when fails" do
      @call_manager.expects(:stream_file).returns(line '-1')
      assert_raise(Exception) { @call_manager.play 'foo' }
    end
  end

  context "capture" do
    {'48' => '0',
     '49' => '1',
     '57' => '9',
     '35' => '#',
     '42' => '*',
     '0' => :timeout}.each do |result, digit|
      should "capture one digit #{digit}" do
        expect_digit result
        value = @call_manager.capture :min => 1, :max => 1, :finish_on_key => '', :timeout => 5
        assert_equal digit, value
      end
     end

    should "capture two digits" do
      expect_digits '42'
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '#', :timeout => 5
      assert_equal '42', value
    end

    should "capture three digits timeout" do
      expect_digits ['4'.ord.to_s, '2'.ord.to_s, '0']
      value = @call_manager.capture :min => 3, :max => 3, :finish_on_key => '#', :timeout => 5
      assert_equal :timeout, value
    end

    should "capture digits finishes on key" do
      expect_digits '42#'
      value = @call_manager.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
      assert_equal '42', value
    end

    should "capture digits and press finish key the first time" do
      expect_digits '#'
      value = @call_manager.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
      assert_equal :finish_key, value
    end

    should "capture digits and timeout" do
      expect_digit '0'
      value = @call_manager.capture :min => 1, :max => 5, :finish_on_key => '#', :timeout => 5
      assert_equal :timeout, value
    end

    should "capture digits while playing" do
      @call_manager.expects(:play).with('some_file', '0123456789#*').returns('4').in_sequence(@seq)
      expect_digit '2'.ord.to_s
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '#', :timeout => 5, :play => 'some_file'
      assert_equal '42', value
    end

    should "capture digits with string values" do
      @call_manager.expects(:play).with('some_file', '0123456789#*').returns('4').in_sequence(@seq)
      expect_digit '2'.ord.to_s
      value = @call_manager.capture :min => '2', :max => '2', :finish_on_key => '#', :timeout => '5', :play => 'some_file'
      assert_equal '42', value
    end

    should "capture digits and play is finish key" do
      @call_manager.expects(:play).with('some_file', '0123456789#*').returns('*').in_sequence(@seq)
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '*', :timeout => 5, :play => 'some_file'
      assert_equal :finish_key, value
    end

    should "capture digits and play nothing pressed" do
      @call_manager.expects(:play).with('some_file', '0123456789#*').returns(nil).in_sequence(@seq)
      expect_digits '24'
      value = @call_manager.capture :min => 2, :max => 2, :finish_on_key => '*', :timeout => 5, :play => 'some_file'
      assert_equal '24', value
    end

    should "capture digits and play just one digit" do
      @call_manager.expects(:play).with('some_file', '0123456789#*').returns('1').in_sequence(@seq)
      value = @call_manager.capture :min => 1, :max => 1, :finish_on_key => '*', :timeout => 5, :play => 'some_file'
      assert_equal '1', value
    end

    def expect_digits(digits)
      if digits.is_a? Array
        digits.each { |digit| expect_digit digit }
      else
        digits.each_char { |c| expect_digit c.ord.to_s }
      end
    end

    def expect_digit(result)
      @call_manager.expects(:wait_for_digit).with(5 * 1000).returns(line result).in_sequence(@seq)
    end

    def line(result)
      stub('line', :result => result)
    end
  end
end