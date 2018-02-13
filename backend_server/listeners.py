
# from backend_server.mq import segmentation_listener
import backend_server.mq

LISTENERS = {
    'queryobj': [{'routing': ['query.requests'],
                  'queue': 'server1',
                  'listener':
                  backend_server.mq.segmentation_listener.on_message}]
}
