---
metaTitle: "Ruby on Rails - ActiveRecord Transactions"
description: "Getting Started with Active Record Transactions"
---

# ActiveRecord Transactions


ActiveRecord Transactions are protective blocks where sequence of active record queries are only permanent if they can all succeed as one atomic action.



## Getting Started with Active Record Transactions


Active Record Transactions can be applied to Model classes as well as Model instances, the objects within the transaction block need not all be instances of same class. This is because transactions are per-database connection, not per-model. For example:

```ruby
User.transaction do
  account.save!
  profile.save!
  print "All saves success, returning 1"
  return 1
end
rescue_from ActiveRecord::RecordInvalid do |exception|
  print "Exception thrown, transaction rolledback"
  render_error "failure", exception.record.errors.full_messages.to_sentence
end

```

Using save with a bang ensures that transaction will be automatically rolled back when the exception is thrown and after the rollback, control goes to the rescue block for the exception. **Make sure you rescue the exceptions thrown from the save! in Transaction Block.**

If you don't want to use save!, you can manually raise `raise ActiveRecord::Rollback` when the save fails. You need not handle this exception. It will then rollback the transaction and take the control to the next statement after transaction block.

```

  User.transaction do
      if account.save && profile.save
        print "All saves success, returning 1"
        return 1
      else
        raise ActiveRecord::Rollback
      end
    end
    print "Transaction Rolled Back"

```

