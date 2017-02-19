from toybox.simpleapi import simple_view


@simple_view("/hello/{name}", request_method="GET")
def hello_world(request):
    return {"message": 'Hello %(name)s!' % request.matchdict}


@simple_view("/hello/{name}", request_method="POST")
def post_world(request):
    return {"message": 'post Hello %(name)s!' % request.matchdict}


@simple_view("/500")
def return_500(request):
    return 10 / 0


if __name__ == '__main__':
    from toybox.simpleapi import run
    run(port=8080)
    # from toybox.shortcuts import cont_proutes
    # run(cont=cont_proutes)
