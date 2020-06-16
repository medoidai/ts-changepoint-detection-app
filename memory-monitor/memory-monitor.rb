require 'platform-api'

require 'open-uri'

$stdout.sync = true

MEMORY_MONITOR_APP_NAME = ENV['MEMORY_MONITOR_APP_NAME']
MEMORY_MONITOR_HEROKU_API_KEY = ENV['MEMORY_MONITOR_HEROKU_API_KEY']

seconds_to_wait_until_next_check = 5 * 60
memory_code = "Error R14"

def shut_down
  puts "[memory-monitor] Shutting down gracefully..."
  sleep 1
end

Signal.trap("INT") {
  shut_down
  exit
}

Signal.trap("TERM") {
  shut_down
  exit
}

raise "[memory-monitor] Provide MEMORY_MONITOR_APP_NAME environment variable!" if MEMORY_MONITOR_APP_NAME.nil? || MEMORY_MONITOR_APP_NAME.empty?

raise "[memory-monitor] Provide MEMORY_MONITOR_HEROKU_API_KEY environment variable!" if MEMORY_MONITOR_HEROKU_API_KEY.nil? || MEMORY_MONITOR_HEROKU_API_KEY.empty?

puts "[memory-monitor] Starting to monitor [#{MEMORY_MONITOR_APP_NAME}]"

heroku = PlatformAPI.connect_oauth(MEMORY_MONITOR_HEROKU_API_KEY)

previous_timestamp = ""

dyno_name = 'web.1'

while true
  begin
    create_response = heroku.log_session.create(MEMORY_MONITOR_APP_NAME, { "dyno" => dyno_name, "lines" => 1000 })

    logs = open(create_response['logplex_url']).readlines.reverse.select{|l| l.include?(memory_code) }

    if logs.length > 0
      latest_timestamp = logs.first.match(/^(\d+-\d+-\d+T\d+:\d+:\d+\.\d+)/)

      if previous_timestamp != latest_timestamp
        puts `supervisorctl pid rshiny | xargs kill -s SIGTERM`

        previous_timestamp = latest_timestamp

        puts "[memory-monitor] Restarting [#{dyno_name}] at [#{Time.now}]"
      end
    end
  rescue => exception
    puts "[memory-monitor] Unable to handle dyno [#{dyno_name}] with error [#{exception.message}]"
  end

  sleep seconds_to_wait_until_next_check
end
