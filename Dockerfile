# https://github.com/virtualstaticvoid/heroku-docker-r#versions
FROM virtualstaticvoid/heroku-docker-r:3.6.3-shiny

# Install Ruby, supervisord
RUN apt-get update -qq \
 && apt-get install -qy ruby supervisor \
 && apt-get autoclean \
 && rm -rf /var/lib/apt/lists/*

# From official Ruby docker image
ENV GEM_HOME="/usr/local/bundle"
ENV BUNDLE_SILENCE_ROOT_WARNING=1
ENV BUNDLE_APP_CONFIG="$GEM_HOME"
ENV PATH="$GEM_HOME/bin:$PATH"

# Install Bundler, and the gems needed in monitor app
RUN gem install bundler -v 2.1.4 \
 && cd /app/memory-monitor \
 && bundle install

ENV PORT=8080

CMD [ "/usr/bin/supervisord", "--nodaemon", "--configuration", "/app/supervisor/supervisord.conf" ]
