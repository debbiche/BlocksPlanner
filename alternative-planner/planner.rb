module BlocksPlanner
  class Planner

  end

  class Action
  end

  class Pick < Action

    def effects(block)
      [Grabber.new(block)]
    end

    def preconditions
      ["grabber empty"]
    end
  end
end