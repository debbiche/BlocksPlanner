module ArrayUtils
  def self_or_first(object)
    if object.respond_to? :each
      return object.first
    else
      return object
    end
  end

  def Array.wrap(o)
    if o.nil?
      return []
    end
    return o.respond_to?(:each) ? o : [o]
  end

  def any
    return self_or_first(yield)
  end

  def the
    return self_or_first(yield)
  end

  def all
    return yield
  end
end