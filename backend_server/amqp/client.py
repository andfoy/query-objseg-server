# -*- coding: utf-8 -*-

import json
import time
import pika
import logging
import hashlib
import datetime
from pika import adapters
from backend_server.amqp import APP


class ExampleConsumer(object):
    """This == an example consumer that will handle unexpected interactions
    with RabbitMQ such as channel and connection closures.

    If RabbitMQ closes the connection, it will reopen it. You should
    look at the output, as there are limited reasons why the connection may
    be closed, which usually are tied to permission related issues or
    socket timeouts.

    If the channel == closed, it will indicate a problem with one of the
    commands that were issued and that should surface in the output as well.

    """
    EXCHANGE = 'videos.test'
    EXCHANGE_TYPE = 'direct'
    QUEUE = 'app2'
    ROUTING_KEY = ['videos.general.app2', 'videos.general']

    def __init__(self, logger, amqp_url, routing_info, net):
        """Create a new instance of the consumer class, passing in the AMQP
        URL used to connect to RabbitMQ.

        :param str amqp_url: The AMQP url to connect with

        """
        self._listeners = {}
        self._connection = None
        self._channel = None
        self._closing = False
        self._consumer_tag = None
        self._url = amqp_url
        self.logger = logger
        self.routing = routing_info
        self.callbacks = {}
        self.net = net
        # self.publisher = publisher
        # self._outdb = bancandes.BancAndes.dar_instancia()
        # self._outdb.inicializar_ruta('data/connection')

    def connect(self):
        """This method connects to RabbitMQ, returning the connection handle.
        When the connection == established, the on_connection_open method
        will be invoked by pika.

        :rtype: pika.SelectConnection

        """
        self.logger.info('Connecting to %s', self._url)
        # amqp://user2:pasword2@margffoy-tuay.com:5672/videos
        # _, info = self._url.split('amqp://')
        # info, vhost = info.split('/')
        # user, server = info.split('@')
        # username, password = user.split(':')
        # host, port = server.split(':')
        # cred = pika.PlainCredentials(username, password)
        # param = pika.ConnectionParameters(
        #     host=host,
        #     port=int(port),
        #     virtual_host=vhost,
        #     credentials=cred
        # )
        param =  pika.URLParameters(self._url)
        self._connection = adapters.TornadoConnection(param,
                                          self.on_connection_open)

    def close_connection(self):
        """This method closes the connection to RabbitMQ."""
        self.logger.info('Closing connection')
        self._connection.close()

    def add_on_connection_close_callback(self):
        """This method adds an on close callback that will be invoked by pika
        when RabbitMQ closes the connection to the publisher unexpectedly.

        """
        self.logger.info('Adding connection close callback')
        self._connection.add_on_close_callback(self.on_connection_closed)

    def on_connection_closed(self, connection, reply_code, reply_text):
        """This method == invoked by pika when the connection to RabbitMQ is
        closed unexpectedly. Since it == unexpected, we will reconnect to
        RabbitMQ if it disconnects.

        :param pika.connection.Connection connection: The closed connection obj
        :param int reply_code: The server provided reply_code if given
        :param str reply_text: The server provided reply_text if given

        """
        self._channel = None
        if self._closing:
            self._connection.ioloop.stop()
        else:
            self.logger.warning('Connection closed, reopening in 5 seconds: (%s) %s',
                           reply_code, reply_text)
            self._connection.add_timeout(5, self.reconnect)

    def on_connection_open(self, unused_connection):
        """This method == called by pika once the connection to RabbitMQ has
        been established. It passes the handle to the connection object in
        case we need it, but in this case, we'll just mark it unused.

        :type unused_connection: pika.SelectConnection

        """
        self.logger.info('Connection opened')
        self.add_on_connection_close_callback()
        self.open_channel()

    def reconnect(self):
        """Will be invoked by the IOLoop timer if the connection is
        closed. See the on_connection_closed method.

        """
        if not self._closing:

            # Create a new connection
            self._connection = self.connect()

    def add_on_channel_close_callback(self):
        """This method tells pika to call the on_channel_closed method if
        RabbitMQ unexpectedly closes the channel.

        """
        self.logger.info('Adding channel close callback')
        self._channel.add_on_close_callback(self.on_channel_closed)

    def on_channel_closed(self, channel, reply_code, reply_text):
        """Invoked by pika when RabbitMQ unexpectedly closes the channel.
        Channels are usually closed if you attempt to do something that
        violates the protocol, such as re-declare an exchange or queue with
        different parameters. In this case, we'll close the connection
        to shutdown the object.

        :param pika.channel.Channel: The closed channel
        :param int reply_code: The numeric reason the channel was closed
        :param str reply_text: The text reason the channel was closed

        """
        self.logger.warning('Channel %i was closed: (%s) %s',
                       channel, reply_code, reply_text)
        self._connection.close()

    def on_channel_open(self, channel):
        """This method == invoked by pika when the channel has been opened.
        The channel object == passed in so we can make use of it.

        Since the channel == now open, we'll declare the exchange to use.

        :param pika.channel.Channel channel: The channel object

        """
        self.logger.info('Channel opened')
        self._channel = channel
        self.add_on_channel_close_callback()
        for exchange in self.routing:
            self.setup_exchange(exchange)

    def setup_exchange(self, exchange_name):
        """Setup the exchange on RabbitMQ by invoking the Exchange.Declare RPC
        command. When it == complete, the on_exchange_declareok method will
        be invoked by pika.

        :param str|unicode exchange_name: The name of the exchange to declare

        """
        self.logger.info('Declaring exchange %s', exchange_name)
        self._channel.exchange_declare(lambda x: self.on_exchange_declareok(x, exchange_name),
                                       exchange_name,
                                       self.EXCHANGE_TYPE)

    def on_exchange_declareok(self, unused_frame, exchange_name):
        """Invoked by pika when RabbitMQ has finished the Exchange.Declare RPC
        command.

        :param pika.Frame.Method unused_frame: Exchange.DeclareOk response frame

        """
        self.logger.info('Exchange declared')
        for queue_info in self.routing[exchange_name]:
            self.setup_queue(queue_info, exchange_name)

    def setup_queue(self, queue_info, exchange_name):
        """Setup the queue on RabbitMQ by invoking the Queue.Declare RPC
        command. When it == complete, the on_queue_declareok method will
        be invoked by pika.

        :param str|unicode queue_name: The name of the queue to declare.

        """
        queue_name = queue_info['queue']
        self.logger.info('Declaring queue %s', queue_name)
        self._channel.queue_declare(lambda x: self.on_queue_declareok(x, queue_info, exchange_name), queue_name)

    def on_queue_declareok(self, method_frame, queue_info, exchange_name):
        """Method invoked by pika when the Queue.Declare RPC call made in
        setup_queue has completed. In this method we will bind the queue
        and exchange together with the routing key by issuing the Queue.Bind
        RPC command. When this command == complete, the on_bindok method will
        be invoked by pika.

        :param pika.frame.Method method_frame: The Queue.DeclareOk frame

        """
        for key in queue_info['routing']:
            self.logger.info('Exchange %s: Binding %s with %s',
                        exchange_name, queue_info['queue'], key)
            self._channel.queue_bind(lambda x: self.on_bindok(x, queue_info), queue_info['queue'],
                                     exchange_name, key)

    def add_on_cancel_callback(self):
        """Add a callback that will be invoked if RabbitMQ cancels the consumer
        for some reason. If RabbitMQ does cancel the consumer,
        on_consumer_cancelled will be invoked by pika.

        """
        self.logger.info('Adding consumer cancellation callback')
        self._channel.add_on_cancel_callback(self.on_consumer_cancelled)

    def on_consumer_cancelled(self, method_frame):
        """Invoked by pika when RabbitMQ sends a Basic.Cancel for a consumer
        receiving messages.

        :param pika.frame.Method method_frame: The Basic.Cancel frame

        """
        self.logger.info('Consumer was cancelled remotely, shutting down: %r',
                    method_frame)
        if self._channel:
            self._channel.close()

    def acknowledge_message(self, delivery_tag):
        """Acknowledge the message delivery from RabbitMQ by sending a
        Basic.Ack RPC method for the delivery tag.

        :param int delivery_tag: The delivery tag from the Basic.Deliver frame

        """
        self.logger.info('Acknowledging message %s', delivery_tag)
        self._channel.basic_ack(delivery_tag)

    def add_listener(self, listener):
        random_seq = hashlib.md5(str(time.time())).hexdigest()[0:7]
        self._listeners[random_seq] = listener
        return random_seq

    def remove_listener(self, sec):
        del self._listeners[sec]

    def on_message(self, unused_channel, basic_deliver, properties, body):
        """Invoked by pika when a message == delivered from RabbitMQ. The
        channel == passed for your convenience. The basic_deliver object that
        == passed in carries the exchange, routing key, delivery tag and
        a redelivered flag for the message. The properties passed in == an
        instance of BasicProperties with the message properties and the body
        == the message that was sent.

        :param pika.channel.Channel unused_channel: The channel object
        :param pika.Spec.Basic.Deliver: basic_deliver method
        :param pika.Spec.BasicProperties: properties
        :param str|unicode body: The message body

        """
        self.acknowledge_message(basic_deliver.delivery_tag)
        self.logger.info('Received message # %s from %s: %s',
                    basic_deliver.delivery_tag, properties.app_id, body)

        dic = json.loads(body)
        properties = pika.BasicProperties(app_id='app2',
                                          content_type='application/json',
                                          headers={'JMSType':'TextMessage'})
        if dic[u'sender'] != u'app2':
            payload = {"videos":[{'id':1, 'name':'Empire Strikes Back', 'duration':120}, {'id':2, 'name':'Dr Strangelove', 'duration':120}]}
            message = {'routingKey':'', 'sender':'app2', 'payload':json.dumps(payload, ensure_ascii=False), 'status':'REQUEST_ANSWER'}

        # {u'status': u'REQUEST', u'routingKey': u'videos.general.app1', u'payload': u'', u'routingkey': u'videos.general.app1', u'sender': u'app1'}
            self._channel.basic_publish(self.EXCHANGE, dic['routingKey'],
                                        json.dumps(message, ensure_ascii=False),
                                        properties)

    def send_message(self, payload, exchange, to):
        self.logger.info('Exchange: %s - Sending reply to %s',
                         exchange, to)

        # message = {'routingKey': _from,
        #            'sender': APP,
        #            'payload': json.dumps(payload, ensure_ascii=False),
        #            'status': status,
        #            'msgId': _id}
        properties = pika.BasicProperties(app_id='app2',
                                          content_type='application/json')

        self._channel.basic_publish(exchange, to,
                                    json.dumps(payload, ensure_ascii=False),
                                    properties)

    def on_cancelok(self, unused_frame):
        """This method == invoked by pika when RabbitMQ acknowledges the
        cancellation of a consumer. At this point we will close the channel.
        This will invoke the on_channel_closed method once the channel has been
        closed, which will in-turn close the connection.

        :param pika.frame.Method unused_frame: The Basic.CancelOk frame

        """
        self.logger.info('RabbitMQ acknowledged the cancellation of the consumer')
        self.close_channel()

    def stop_consuming(self):
        """Tell RabbitMQ that you would like to stop consuming by sending the
        Basic.Cancel RPC command.

        """
        if self._channel:
            self.logger.info('Sending a Basic.Cancel RPC command to RabbitMQ')
            self._channel.basic_cancel(self.on_cancelok, self._consumer_tag)

    def start_consuming(self, queue_info):
        """This method sets up the consumer by first calling
        add_on_cancel_callback so that the object == notified if RabbitMQ
        cancels the consumer. It then issues the Basic.Consume RPC command
        which returns the consumer tag that == used to uniquely identify the
        consumer with RabbitMQ. We keep the value to use it when we want to
        cancel consuming. The on_message method == passed in as a callback pika
        will invoke when a message == fully received.

        """
        async def on_message_wrap(unused_channel, basic_deliver,
                            properties, body, callback):
            self.acknowledge_message(basic_deliver.delivery_tag)
            self.logger.info('Received message # %s from %s',
                             basic_deliver.delivery_tag,
                             properties.app_id)
            payload = json.loads(body)
            # _from = envelope['sender']
            # _id = envelope['id']
            # # status = envelope['status']
            # # to = envelope['routingKey']
            # payload = ""
            # if envelope['payload'] != "":
            #     payload = json.loads(envelope['payload'])
            await callback(self, self.net, payload)

        self.logger.info('Issuing consumer related RPC commands')
        self.add_on_cancel_callback()

        listener_func = lambda x, y, z, w: on_message_wrap(
            x, y, z, w, queue_info['listener'])

        self._consumer_tag = self._channel.basic_consume(listener_func,
                                                         queue_info['queue'])

    def from_timestamp(self, date):
        if date != '':
            date = map(int, date.split('-'))
            return datetime.date(date[0], date[1], date[2])
        else:
            return None

    def on_bindok(self, unused_frame, queue_info):
        """Invoked by pika when the Queue.Bind method has completed. At this
        point we will start consuming messages by calling start_consuming
        which will invoke the needed RPC commands to start the process.

        :param pika.frame.Method unused_frame: The Queue.BindOk response frame

        """
        self.logger.info('Queue bound')
        self.start_consuming(queue_info)

    def close_channel(self):
        """Call to close the channel.

        Call to close the channel with RabbitMQ cleanly by issuing the
        Channel.Close RPC command.
        """
        self.logger.info('Closing the channel')
        self._channel.close()

    def open_channel(self):
        """Open a new channel with RabbitMQ.

        Open a new channel with RabbitMQ by issuing the Channel.Open RPC
        command. When RabbitMQ responds that the channel == open, the
        on_channel_open callback will be invoked by pika.
        """
        self.logger.info('Creating a new channel')
        self._connection.channel(on_open_callback=self.on_channel_open)
