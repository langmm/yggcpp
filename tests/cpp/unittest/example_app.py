import os
from flask import Flask, request
import multiprocessing


app = Flask(__name__)
app.queue = {}


@app.route('/<client_id>/<model>/<channel>',
           methods=['GET', 'PUT', 'POST'])
def queue(client_id, model, channel):
    r"""Respond to GET with queued message and add message from PUT
    or POST to the queue."""
    if request.method in ['PUT', 'POST']:
        # Queue a message when it is received from a client.
        app.queue.setdefault((client_id, model, channel), [])
        msg = request.get_data()
        app.queue[(client_id, model, channel)].append(msg)
        return b''
    else:
        # Return a message from the queue when requested by a client.
        if app.queue.get((client_id, model, channel), []):
            msg = app.queue[(client_id, model, channel)].pop(0)
            return msg
        else:
            return b''


@app.route('/<client_id>/<model>/<channel>/size', methods=['GET'])
def queue_size(client_id, model, channel):
    r"""Return the size of the message queue."""
    return str(len(app.queue.get((client_id, model, channel), [])))


@app.route('/<client_id>/<model>/<channel>/remove', methods=['GET'])
def queue_remove(client_id, model, channel):
    r"""Remove a queue."""
    app.queue.pop((client_id, model, channel), None)
    return b''


def run_server(q):

    @app.route('/startup')
    def startup():
        print("SERVER RECEIVED startup")
        return 'startup'

    @app.route('/shutdown')
    def shutdown():
        print("SERVER RECEIVED shutdown")
        q.put('shutdown')
        return 'shutdown'

    print("RUNNING FLASK SERVER APPLICATION")
    app.run(host='localhost', port=int(os.environ.get('PORT', 5000)))


def run_server_terminate():
    q = multiprocessing.Queue()
    p = multiprocessing.Process(target=run_server, args=(q,))
    p.start()
    print("STARTED FLASK SERVER APPLICATION")
    token = q.get(block=True)
    print("SHUTDOWN FLASK SERVER APPLICATION")
    if p.is_alive():
        p.terminate()
    if p.is_alive():
        p.kill()
    if p.is_alive():
        p.join(1)
    assert not p.is_alive()
    p.close()
    return token


if __name__ == '__main__':
    run_server_terminate()
