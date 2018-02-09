
from logic import mq


LISTENERS = {
    'queryobj': [{'routing': ['query.requests'],
                  'queue': 'server1',
                  'listener': mq.videos_remote.on_message}]
}
