class ATM
  attr_reader :state

  def initialize
    @state = :ready
  end

  def from_states(states, &block)
    @state = if !states.include?(@state)
               warn("Action not allowed. State needs to be one of: #{states.inspect}, curr=#{@state.inspect}")
               @state
             else
               new_state = block.call
               warn("New state: #{new_state.inspect}\n")
               new_state
             end
    self
  end

  def insert_card
    from_states([:ready]) do
      puts 'Card inserted. Please enter your PIN.'
      :validating_card
    end
  end

  def verify_card!
    from_states([:validating_card]) do
      # if rand < 0.5  then
      #   puts 'This card is invalid, ejecting!'
      #   :ready
      # else
      #   puts 'This card is known, proceeding...'
      :validating_pin
      # end
    end
  end

  def verify_pin!(pin)
    from_states([:validating_pin]) do
      if pin == '123'
        puts 'PIN accepted! Proceeding'
        :session
      else
        puts 'Invalid PIN. Please try again.'
        :validating_pin
      end
    end
  end

  def withdraw
    from_states([:session]) do
      puts "Withdrawing X amount..."
      :session
    end
  end

  def request_eject
    from_states([:validating_pin, :session]) do
      puts 'Card ejected. Thank you for using the ATM.'
      :ready
    end
  end
end

# Example 1:
atm = ATM.new
atm.insert_card
atm.verify_card!
atm.verify_pin!('123')
atm.withdraw
atm.request_eject
p(example: 1, final_state: atm.state)


# Example 2:
atm = ATM.new
         .insert_card
         .verify_card!
         .verify_pin!('123')
         .withdraw
         .request_eject
p(example: 2, final_state: atm.state)