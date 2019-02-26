function processParameter(param, op, path, method, index, openapi, options) {
    let result = {};
    let singularRequestBody = true;
    let originalType;

    if (op && op.consumes && (typeof op.consumes === 'string')) {
        if (options.patch) {
            op.consumes = [op.consumes];
        }
        else {
            return throwError('(Patchable) operation.consumes must be an array', options);
        }
    }
    if (!Array.isArray(openapi.consumes)) delete openapi.consumes;
    let consumes = ((op ? op.consumes : null) || (openapi.consumes || [])).filter(common.uniqueOnly);

    if (param && param.$ref && (typeof param.$ref === 'string')) {
        // if we still have a ref here, it must be an internal one
        fixParamRef(param, options);
        let ptr = decodeURIComponent(param.$ref.replace('#/components/parameters/', ''));
        let rbody = false;
        let target = openapi.components.parameters[ptr]; // resolves a $ref, must have been sanitised already

        if (((!target) || (target["x-s2o-delete"])) && param.$ref.startsWith('#/')) {
            // if it's gone, chances are it's a requestBody component now unless spec was broken
            param["x-s2o-delete"] = true;
            rbody = true;
        }

        // shared formData parameters from swagger or path level could be used in any combination.
        // we dereference all op.requestBody's then hash them and pull out common ones later

        if (rbody) {
            let ref = param.$ref;
            let newParam = resolveInternal(openapi, param.$ref);
            if (!newParam && ref.startsWith('#/')) {
                throwOrWarn('Could not resolve reference ' + ref, param, options);
            }
            else {
                if (newParam) param = newParam; // preserve reference
            }
        }
    }

    if (param && (param.name || param.in)) { // if it's a real parameter OR we've dereferenced it

        if (typeof param['x-deprecated'] === 'boolean') {
            param.deprecated = param['x-deprecated'];
            delete param['x-deprecated'];
        }

        if (typeof param['x-example'] !== 'undefined') {
            param.example = param['x-example'];
            delete param['x-example'];
        }

        if ((param.in != 'body') && (!param.type)) {
            if (options.patch) {
                param.type = 'string';
            }
            else {
                throwError('(Patchable) parameter.type is mandatory for non-body parameters', options);
            }
        }
        if (param.type && typeof param.type === 'object' && param.type.$ref) {
            // $ref anywhere sensibility
            param.type = resolveInternal(openapi, param.type.$ref);
        }
        if (param.type === 'file') {
            param['x-s2o-originalType'] = param.type;
            originalType = param.type;
        }
        if (param.description && typeof param.description === 'object' && param.description.$ref) {
            // $ref anywhere sensibility
            param.description = resolveInternal(openapi, param.description.$ref);
        }
        if (param.description === null) delete param.description;

        let oldCollectionFormat = param.collectionFormat;
        if (param.collectionFormat) {
            if (param.type != 'array') {
                if (options.patch) {
                    delete param.collectionFormat;
                }
                else {
                    throwError('(Patchable) collectionFormat is only applicable to param.type array', options);
                }
            }
            if ((param.collectionFormat === 'csv') && ((param.in === 'query') || (param.in === 'cookie'))) {
                param.style = 'form';
                param.explode = false;
            }
            if ((param.collectionFormat === 'csv') && ((param.in === 'path') || (param.in === 'header'))) {
                param.style = 'simple';
            }
            if (param.collectionFormat === 'ssv') {
                if (param.in === 'query') {
                    param.style = 'spaceDelimited';
                }
                else {
                    throwOrWarn('collectionFormat:ssv is no longer supported except for in:query parameters', param, options); // not lossless
                }
            }
            if (param.collectionFormat === 'pipes') {
                if (param.in === 'query') {
                    param.style = 'pipeDelimited';
                }
                else {
                    throwOrWarn('collectionFormat:pipes is no longer supported except for in:query parameters', param, options); // not lossless
                }
            }
            if (param.collectionFormat === 'multi') {
                param.explode = true;
            }
            if (param.collectionFormat === 'tsv') {
                throwOrWarn('collectionFormat:tsv is no longer supported', param, options); // not lossless
                param["x-collectionFormat"] = 'tsv';
            }
            delete param.collectionFormat;
        }

        if (param.type && (param.type != 'object') && (param.type != 'body') && (param.in != 'formData')) {
            if (param.items && param.schema) {
                throwOrWarn('parameter has array,items and schema', param, options);
            }
            else {
                if ((!param.schema) || (typeof param.schema !== 'object')) param.schema = {};
                param.schema.type = param.type;
                if (param.items) {
                    param.schema.items = param.items;
                    delete param.items;
                    recurse(param.schema.items, null, function (obj, key, state) {
                        if ((key === 'collectionFormat') && (typeof obj[key] === 'string')) {
                            if (oldCollectionFormat && obj[key] !== oldCollectionFormat) {
                                throwOrWarn('Nested collectionFormats are not supported', param, options);
                            }
                            delete obj[key]; // not lossless
                        }
                        // items in 2.0 was a subset of JSON-Schema items object, it gets
                        // fixed up below
                    });
                }
                for (let prop of common.parameterTypeProperties) {
                    if (typeof param[prop] !== 'undefined') param.schema[prop] = param[prop];
                    delete param[prop];
                }
            }
        }

        if (param.schema) {
            fixUpSchema(param.schema,options);
        }

        if (param["x-ms-skip-url-encoding"]) {
            if (param.in === 'query') { // might be in:path, not allowed in OAS3
                param.allowReserved = true;
                delete param["x-ms-skip-url-encoding"];
            }
        }
    }

    if (param && param.in === 'formData') {
        // convert to requestBody component
        singularRequestBody = false;
        result.content = {};
        let contentType = 'application/x-www-form-urlencoded';
        if ((consumes.length) && (consumes.indexOf('multipart/form-data') >= 0)) {
            contentType = 'multipart/form-data';
        }

        result.content[contentType] = {};
        if (param.schema) {
            result.content[contentType].schema = param.schema;
            if (param.schema.$ref) {
                result['x-s2o-name'] = decodeURIComponent(param.schema.$ref.replace('#/components/schemas/', ''));
            }
        }
        else {
            result.content[contentType].schema = {};
            result.content[contentType].schema.type = 'object';
            result.content[contentType].schema.properties = {};
            result.content[contentType].schema.properties[param.name] = {};
            let schema = result.content[contentType].schema;
            let target = result.content[contentType].schema.properties[param.name];
            if (param.description) target.description = param.description;
            if (param.example) target.example = param.example;
            if (param.type) target.type = param.type;

            for (let prop of common.parameterTypeProperties) {
                if (typeof param[prop] !== 'undefined') target[prop] = param[prop];
            }
            if (param.required === true) {
                if (!schema.required) schema.required = [];
                schema.required.push(param.name);
            }
            if (typeof param.default !== 'undefined') target.default = param.default;
            if (target.properties) target.properties = param.properties;
            if (param.allOf) target.allOf = param.allOf; // new are anyOf, oneOf, not
            if ((param.type === 'array') && (param.items)) {
                target.items = param.items;
                if (target.items.collectionFormat) delete target.items.collectionFormat;
            }
            if ((originalType === 'file') || (param['x-s2o-originalType'] === 'file')) {
                target.type = 'string';
                target.format = 'binary';
            }

            // Copy any extensions on the form param to the target schema property.
            copyExtensions(param, target);
        }
    }
    else if (param && param.type === 'file') {
        // convert to requestBody
        if (param.required) result.required = param.required;
        result.content = {};
        result.content["application/octet-stream"] = {};
        result.content["application/octet-stream"].schema = {};
        result.content["application/octet-stream"].schema.type = 'string';
        result.content["application/octet-stream"].schema.format = 'binary';
        copyExtensions(param, result);
    }
    if (param && param.in === 'body') {
        result.content = {};
        if (param.name) result['x-s2o-name'] = (op && op.operationId ? common.sanitiseAll(op.operationId) : '') + ('_' + param.name).toCamelCase();
        if (param.description) result.description = param.description;
        if (param.required) result.required = param.required;

        // Set the "request body name" extension on the operation if requested.
        if (op && options.rbname && param.name) {
            op[options.rbname] = param.name;
        }
        if (param.schema && param.schema.$ref) {
            result['x-s2o-name'] = decodeURIComponent(param.schema.$ref.replace('#/components/schemas/', ''));
        }
        else if (param.schema && (param.schema.type === 'array') && param.schema.items && param.schema.items.$ref) {
            result['x-s2o-name'] = decodeURIComponent(param.schema.items.$ref.replace('#/components/schemas/', '')) + 'Array';
        }

        if (!consumes.length) {
            consumes.push('application/json'); // TODO verify default
        }

        for (let mimetype of consumes) {
            result.content[mimetype] = {};
            result.content[mimetype].schema = clone(param.schema || {});
            fixUpSchema(result.content[mimetype].schema,options);
        }

        // Copy any extensions from the original parameter to the new requestBody
        copyExtensions(param, result);
    }

    if (Object.keys(result).length > 0) {
        param["x-s2o-delete"] = true;
        // work out where to attach the requestBody
        if (op) {
            if (op.requestBody && singularRequestBody) {
                op.requestBody["x-s2o-overloaded"] = true;
                let opId = op.operationId || index;

                throwOrWarn('Operation ' + opId + ' has multiple requestBodies', op, options);
            }
            else {
                if (!op.requestBody) {
                   op = path[method] = attachRequestBody(op,options); // make sure we have one
                }
                if ((op.requestBody.content && op.requestBody.content["multipart/form-data"])
                    && (op.requestBody.content["multipart/form-data"].schema) && (op.requestBody.content["multipart/form-data"].schema.properties) && (result.content["multipart/form-data"]) && (result.content["multipart/form-data"].schema) && (result.content["multipart/form-data"].schema.properties)) {
                    op.requestBody.content["multipart/form-data"].schema.properties =
                        Object.assign(op.requestBody.content["multipart/form-data"].schema.properties, result.content["multipart/form-data"].schema.properties);
                    op.requestBody.content["multipart/form-data"].schema.required = (op.requestBody.content["multipart/form-data"].schema.required || []).concat(result.content["multipart/form-data"].schema.required||[]);
                    if (!op.requestBody.content["multipart/form-data"].schema.required.length) {
                        delete op.requestBody.content["multipart/form-data"].schema.required;
                    }
                }
                else if ((op.requestBody.content && op.requestBody.content["application/x-www-form-urlencoded"] && op.requestBody.content["application/x-www-form-urlencoded"].schema && op.requestBody.content["application/x-www-form-urlencoded"].schema.properties)
                    && result.content["application/x-www-form-urlencoded"] && result.content["application/x-www-form-urlencoded"].schema && result.content["application/x-www-form-urlencoded"].schema.properties) {
                    op.requestBody.content["application/x-www-form-urlencoded"].schema.properties =
                        Object.assign(op.requestBody.content["application/x-www-form-urlencoded"].schema.properties, result.content["application/x-www-form-urlencoded"].schema.properties);
                    op.requestBody.content["application/x-www-form-urlencoded"].schema.required = (op.requestBody.content["application/x-www-form-urlencoded"].schema.required || []).concat(result.content["application/x-www-form-urlencoded"].schema.required||[]);
                    if (!op.requestBody.content["application/x-www-form-urlencoded"].schema.required.length) {
                        delete op.requestBody.content["application/x-www-form-urlencoded"].schema.required;
                    }
                }
                else {
                    op.requestBody = Object.assign(op.requestBody, result);
                    if (!op.requestBody['x-s2o-name']) {
                        if (op.requestBody.schema && op.requestBody.schema.$ref) {
                            op.requestBody['x-s2o-name'] = decodeURIComponent(op.requestBody.schema.$ref.replace('#/components/schemas/', '')).split('/').join('');
                        }
                        else if (op.operationId) {
                            op.requestBody['x-s2o-name'] = common.sanitiseAll(op.operationId);
                        }
                    }
                }
            }
        }
    }

    // tidy up
    if (param && !param['x-s2o-delete']) {
        delete param.type;
        for (let prop of common.parameterTypeProperties) {
            delete param[prop];
        }

        if ((param.in === 'path') && ((typeof param.required === 'undefined') || (param.required !== true))) {
            if (options.patch) {
                param.required = true;
            }
            else {
                throwError('(Patchable) path parameters must be required:true ['+param.name+' in '+index+']', options);
            }
        }
    }

    return op;
}
