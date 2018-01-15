import os
from logic import mq

LISTENERS = {
    'videos.test':[
        {
            'routing':['videos.general', 'videos.general.app2'], 
            'queue':'app2videosgeneral', 
            'listener':mq.videos_remote.on_message
        },
    ]
}