
from backend_server.mq import segmentation_listener

LISTENERS = {
    'queryobj': [{'routing': ['query.requests'],
                  'queue': 'server1',
                  'listener': segmentation_listener.on_message}]
}
