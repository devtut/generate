---
metaTitle: "Ruby on Rails - Testing Rails Applications"
description: "Unit Test, Request Test"
---

# Testing Rails Applications



## Unit Test


Unit tests test parts of the application in isolation. usually a unit under test is a class or module.

```ruby
let(:gift) { create :gift }

describe '#find' do
  subject { described_class.find(user, Time.zone.now.to_date) }
  it { is_expected.to eq gift }
end

```

[source](https://github.com/24pullrequests/24pullrequests/blob/master/spec/models/gift_spec.rb#L14-L17)

This kind if test is as direct and specific as possible.



## Request Test


Request tests are end to end tests that imitate the behavior of a user.

```ruby
it 'allows the user to set their preferences' do
  check 'Ruby'
  click_on 'Save and Continue'
  expect(user.languages).to eq ['Ruby']
end

```

[source](https://github.com/24pullrequests/24pullrequests/blob/master/spec/requests/dashboard_spec.rb#L53-L58)

This kind of test focuses on user flows and runs through all layers of the system sometimes even rendering javascript.

