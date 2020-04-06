/*
package play.modules.swagger

import com.fasterxml.jackson.databind.JavaType
import io.swagger.annotations._
import io.swagger.annotations.Info
import io.swagger.converter.ModelConverters
import io.swagger.models.Contact
import io.swagger.models.ExternalDocs
import io.swagger.models._
import io.swagger.models.Tag
import io.swagger.models.auth.In
import io.swagger.models.parameters._
import io.swagger.models.parameters.Parameter
import io.swagger.models.properties._
import io.swagger.util._
import org.apache.commons.lang3.StringUtils
import play.Logger
//import play.modules.swagger.util.CrossUtil
import play.routes.compiler.{Parameter, _}

import scala.Option
import java.lang.annotation.Annotation
import java.lang.reflect.Method
import java.lang.reflect.Type
import java.util._
import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.immutable

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._
import scala.collection.Iterator


object PlayReader {
  private val SUCCESSFUL_OPERATION = "successful operation"

  private def typeFromString(`type`: String, cls: Class[_]): Type = {
    val primitive = PrimitiveType.fromName(`type`)
    if (primitive != null) return primitive.getKeyClass
    try {
      val routeType = getOptionTypeFromString(`type`, cls)
      if (routeType != null) return routeType
      return Thread.currentThread.getContextClassLoader.loadClass(`type`)
    } catch {
      case e: Exception =>
        Logger.of("swagger").error(String.format("Failed to resolve '%s' into class", `type`), e)
    }
    null
  }

  private[swagger] object OptionTypeResolver {
    private[swagger] def resolveOptionType(innerType: String, cls: Class[_]) = try
      Json.mapper.getTypeFactory.constructType(classOf[PlayReader.OptionTypeResolver].getDeclaredField("optionType" + innerType).getGenericType, cls)
    catch {
      case e: NoSuchFieldException =>
        null
    }
  }

  final private[swagger] class OptionTypeResolver {
    private val optionTypeInt = null
    private val optionTypeLong = null
    private val optionTypeByte = null
    private val optionTypeBoolean = null
    private val optionTypeChar = null
    private val optionTypeFloat = null
    private val optionTypeDouble = null
    private val optionTypeShort = null
  }

  private def getOptionTypeFromString(simpleTypeName: String, cls: Class[_]): Type = {
    if (simpleTypeName == null) return null
    val regex = "(Option|scala\\.Option)\\s*\\[\\s*(Int|Long|Float|Double|Byte|Short|Char|Boolean)\\s*\\]\\s*$"
    val pattern = Pattern.compile(regex)
    val matcher = pattern.matcher(simpleTypeName)
    if (matcher.find) {
      val enhancedType = matcher.group(2)
      OptionTypeResolver.resolveOptionType(enhancedType, cls)
    }
    else null
  }

  private def parseSchemes(schemes: String) = {
    val result = util.EnumSet.noneOf(classOf[Scheme])
    for (item <- StringUtils.trimToEmpty(schemes).split(",")) {
      val scheme = Scheme.forValue(StringUtils.trimToNull(item))
      if (scheme != null) result.add(scheme)
    }
    result
  }

  private def isVoid(`type`: Type) = {
    val cls = Json.mapper.getTypeFactory.constructType(`type`).getRawClass
    classOf[Void].isAssignableFrom(cls) || Void.TYPE.isAssignableFrom(cls)
  }

  private def isValidResponse(`type`: Type): Boolean = {
    if (`type` == null) return false
    val javaType = Json.mapper.getTypeFactory.constructType(`type`)
    if (isVoid(javaType)) return false
    val cls = javaType.getRawClass
    !isResourceClass(cls)
  }

  private def isResourceClass(cls: Class[_]) = cls.getAnnotation(classOf[Api]) != null

  private[swagger] object ContainerWrapper extends util.Enumeration {
    type ContainerWrapper = Value
    val LIST, ARRAY, MAP, SET = Value
    private val container = nulldef
    this (container: String) {
      this ()
      this.container = container
    }
    def wrapContainer(container: String, property: Property, allowed: PlayReader.ContainerWrapper *
                     ): Property = {
      val tmp = if (allowed.length > 0) util.EnumSet.copyOf(util.Arrays.asList(allowed))
      else util.EnumSet.allOf(classOf[PlayReader.ContainerWrapper])
      import scala.collection.JavaConversions._
      for (wrapper <- tmp) {
        val prop = wrapper.wrap(container, property)
        if (prop != null) return prop
      }
      property
    }
    def wrap(container: String, property: Property): Property
    =
    {
      if (this.container.equalsIgnoreCase(container)) return doWrap(property)
      null
    }
    protected

    def doWrap(property: Property): Property
  }

}

class PlayReader {

  private var config: PlaySwaggerConfig = _
  private var routes: RouteWrapper = _
  private var swagger: Swagger = _

  def getSwagger: Swagger = swagger

  def this(config: PlaySwaggerConfig, routes: RouteWrapper, swagger: Swagger) = {
    this
    this.config = config
    this.routes = routes
    this.swagger = if (swagger == null) new Swagger else swagger
  }


  def read(classes: java.util.Set[Class[_]]): Swagger = {
    // process SwaggerDefinitions first - so we get tags in desired order
    for (cls <- classes.asScala) {
      val swaggerDefinition = cls.getAnnotation(classOf[SwaggerDefinition])
      if (swaggerDefinition != null) readSwaggerConfig(cls, swaggerDefinition)
    }
    for (cls <- classes.asScala) {
      read(cls)
    }
    swagger
  }

  def read(cls: Class[_]): Swagger = read(cls, readHidden = false)

  private def read(cls: Class[_], readHidden: Boolean) = {
    val api = cls.getAnnotation(classOf[Api])

    val tags = new java.util.HashMap[String, Tag]
    val securities = new java.util.ArrayList[SecurityRequirement]
    var consumes = new Array[String](0)
    var produces = new Array[String](0)
    val globalSchemes = java.util.EnumSet.noneOf(classOf[Scheme])

    val readable = (api != null && readHidden) || (api != null && !api.hidden)

    // TODO possibly allow parsing also without @Api annotation
    if (readable) { // the value will be used as a tag for 2.0 UNLESS a Tags annotation is present
      val tagStrings = extractTags(api)
      for (tagString <- tagStrings.asScala) {
        val tag = new Tag().name(tagString)
        tags.put(tagString, tag)
      }
      for (tagName <- tags.keySet.asScala) {
        getSwagger.tag(tags.get(tagName))
      }
      if (!api.produces.isEmpty) {
        produces = toArray(api.produces)
      }
      if (!api.consumes.isEmpty) {
        consumes = toArray(api.consumes)
      }
      globalSchemes.addAll(PlayReader.parseSchemes(api.protocols))
      val authorizations = api.authorizations
      for (auth <- authorizations) {
        if (auth.value != null && !("" == auth.value)) {
          val security = new SecurityRequirement
          security.setName(auth.value)
          val scopes = auth.scopes
          for (scope <- scopes) {
            if (scope.scope != null && !("" == scope.scope)) security.addScope(scope.scope)
          }
          securities.add(security)
        }
      }
      // parse the method
      val methods = cls.getMethods
      for (method <- methods) {
        if (ReflectionUtils.isOverriddenMethod(method, cls)) continue //todo: continue is not supported
        // complete name as stored in route
        val fullMethodName = getFullMethodName(cls, method)
        if (!routes.exists(fullMethodName)) continue //todo: continue is not supported
        val route = routes.apply(fullMethodName)
        val operationPath = getPathFromRoute(route.path, config.basePath)
        if (operationPath != null) {
          val apiOperation = ReflectionUtils.getAnnotation(method, classOf[ApiOperation])
          val httpMethod = extractOperationMethod(apiOperation, method, route)
          var operation = null
          if (apiOperation != null || httpMethod != null) operation = parseMethod(cls, method, route)
          if (operation == null) continue //todo: continue is not supported
          if (apiOperation != null) {
            import scala.collection.JavaConversions._
            for (scheme <- PlayReader.parseSchemes(apiOperation.protocols)) {
              operation.scheme(scheme)
            }
          }
          if (operation.getSchemes == null || operation.getSchemes.isEmpty) {
            import scala.collection.JavaConversions._
            for (scheme <- globalSchemes) {
              operation.scheme(scheme)
            }
          }
          // can't continue without a valid http method
          if (httpMethod != null) {
            if (apiOperation != null) {
              for (tag <- apiOperation.tags) {
                if (!("" == tag)) {
                  operation.tag(tag)
                  getSwagger.tag(new Tag().name(tag))
                }
              }
              operation.getVendorExtensions.putAll(BaseReaderUtils.parseExtensions(apiOperation.extensions))
            }
            if (operation.getConsumes == null) for (mediaType <- consumes) {
              operation.consumes(mediaType)
            }
            if (operation.getProduces == null) for (mediaType <- produces) {
              operation.produces(mediaType)
            }
            if (operation.getTags == null) {
              import scala.collection.JavaConversions._
              for (tagString <- tags.keySet) {
                operation.tag(tagString)
              }
            }
            // Only add global @Api securities if operation doesn't already have more
            // specific securities
            if (operation.getSecurity == null) {
              import scala.collection.JavaConversions._
              for (security <- securities) {
                operation.security(security)
              }
            }
            var path = getSwagger.getPath(operationPath)
            if (path == null) {
              path = new Path
              getSwagger.path(operationPath, path)
            }
            path.set(httpMethod, operation)
            try
              readImplicitParameters(method, operation, cls)
            catch {
              case e: Exception =>
                throw e
            }
          }
        }
      }
    }
    getSwagger
  }

  private[swagger] def getPathFromRoute(pathPattern: PathPattern, basePath: String) = {
    val sb = new StringBuilder
    val iter = pathPattern.parts.iterator
    while ( {
      iter.hasNext
    }) {
      val part = iter.next.asInstanceOf[PathPart]
      if (part.isInstanceOf[StaticPart]) sb.append(part.asInstanceOf[StaticPart].value)
      else if (part.isInstanceOf[DynamicPart]) {
        sb.append("{")
        sb.append(part.asInstanceOf[DynamicPart].name)
        sb.append("}")
      }
      else try
        sb.append(part.asInstanceOf[StaticPart].value)
      catch {
        case e: ClassCastException =>
          Logger.of("swagger").warn(String.format("ClassCastException parsing path from route: %s", e.getMessage))
      }
    }
    val basePathFilter = new StringBuilder(basePath)
    if (basePath.startsWith("/")) basePathFilter.deleteCharAt(0)
    if (!basePath.endsWith("/")) basePathFilter.append("/")
    val basePathString = basePathFilter.toString
    val pathPatternString = sb.toString
    val operationPath = new StringBuilder
    if ((pathPatternString.startsWith("/") && pathPatternString.startsWith(basePathString, 1)) || pathPatternString.startsWith(basePathString)) operationPath.append(pathPatternString.replaceFirst(basePathString, ""))
    else operationPath.append(pathPatternString)
    if (!operationPath.toString.startsWith("/")) operationPath.insert(0, "/")
    operationPath.toString
  }

  protected def readSwaggerConfig(cls: Class[_], config: SwaggerDefinition): Unit = {
    if (!config.basePath.isEmpty) swagger.setBasePath(config.basePath)
    if (!config.host.isEmpty) swagger.setHost(config.host)
    readInfoConfig(config)
    for (consume <- config.consumes) {
      if (StringUtils.isNotEmpty(consume)) swagger.addConsumes(consume)
    }
    for (produce <- config.produces) {
      if (StringUtils.isNotEmpty(produce)) swagger.addProduces(produce)
    }
    if (!config.externalDocs.value.isEmpty) {
      var externalDocs = swagger.getExternalDocs
      if (externalDocs == null) {
        externalDocs = new ExternalDocs
        swagger.setExternalDocs(externalDocs)
      }
      externalDocs.setDescription(config.externalDocs.value)
      if (!config.externalDocs.url.isEmpty) externalDocs.setUrl(config.externalDocs.url)
    }
    for (tagConfig <- config.tags) {
      if (!tagConfig.name.isEmpty) {
        val tag = new Tag
        tag.setName(tagConfig.name)
        tag.setDescription(tagConfig.description)
        if (!tagConfig.externalDocs.value.isEmpty) tag.setExternalDocs(new ExternalDocs(tagConfig.externalDocs.value, tagConfig.externalDocs.url))
        tag.getVendorExtensions.putAll(BaseReaderUtils.parseExtensions(tagConfig.extensions))
        swagger.addTag(tag)
      }
    }
    for (ann <- config.securityDefinition.apiKeyAuthDefinitions) {
      val defn = new ApiKeyAuthDefinition
      defn.setName(ann.name)
      defn.setIn(In.forValue(ann.in.toValue))
      defn.setDescription(ann.description)
      swagger.addSecurityDefinition(ann.key, defn)
    }
    for (ann <- config.securityDefinition.basicAuthDefinitions) {
      val defn = new BasicAuthDefinition
      defn.setDescription(ann.description)
      swagger.addSecurityDefinition(ann.key, defn)
    }
    for (ann <- config.securityDefinition.oAuth2Definitions) {
      val defn = new OAuth2Definition
      defn.setTokenUrl(ann.tokenUrl)
      defn.setAuthorizationUrl(ann.authorizationUrl)
      defn.setFlow(ann.flow.name.toLowerCase)
      for (scope <- ann.scopes) {
        defn.addScope(scope.name, scope.description)
      }
      swagger.addSecurityDefinition(ann.key, defn)
    }
    for (scheme <- config.schemes) {
      if (scheme ne SwaggerDefinition.Scheme.DEFAULT) swagger.addScheme(Scheme.forValue(scheme.name))
    }
  }

  protected def readInfoConfig(config: SwaggerDefinition): Unit = {
    val infoConfig = config.info
    var info = swagger.getInfo
    if (info == null) {
      info = new Info
      swagger.setInfo(info)
    }
    if (!infoConfig.description.isEmpty) info.setDescription(infoConfig.description)
    if (!infoConfig.termsOfService.isEmpty) info.setTermsOfService(infoConfig.termsOfService)
    if (!infoConfig.title.isEmpty) info.setTitle(infoConfig.title)
    if (!infoConfig.version.isEmpty) info.setVersion(infoConfig.version)
    if (!infoConfig.contact.name.isEmpty) {
      var contact = info.getContact
      if (contact == null) {
        contact = new Contact
        info.setContact(contact)
      }
      contact.setName(infoConfig.contact.name)
      if (!infoConfig.contact.email.isEmpty) contact.setEmail(infoConfig.contact.email)
      if (!infoConfig.contact.url.isEmpty) contact.setUrl(infoConfig.contact.url)
    }
    if (!infoConfig.license.name.isEmpty) {
      var license = info.getLicense
      if (license == null) {
        license = new License
        info.setLicense(license)
      }
      license.setName(infoConfig.license.name)
      if (!infoConfig.license.url.isEmpty) license.setUrl(infoConfig.license.url)
    }
    info.getVendorExtensions.putAll(BaseReaderUtils.parseExtensions(infoConfig.extensions))
  }

  private def readImplicitParameters(method: Method, operation: Operation, cls: Class[_]): Unit = {
    val implicitParams = method.getAnnotation(classOf[ApiImplicitParams])
    if (implicitParams != null && implicitParams.value.length > 0) for (param <- implicitParams.value) {
      val p = readImplicitParam(param, cls)
      if (p != null) operation.addParameter(p)
    }
  }

  protected def readImplicitParam(param: ApiImplicitParam, cls: Class[_]): Parameter = {
    var p = null
    if (param.paramType.equalsIgnoreCase("path")) p = new PathParameter
    else if (param.paramType.equalsIgnoreCase("query")) p = new QueryParameter
    else if (param.paramType.equalsIgnoreCase("form") || param.paramType.equalsIgnoreCase("formData")) p = new FormParameter
    else if (param.paramType.equalsIgnoreCase("body")) p = null
    else if (param.paramType.equalsIgnoreCase("header")) p = new HeaderParameter
    else {
      Logger.of("swagger").warn("Unkown implicit parameter type: [" + param.paramType + "]")
      return null
    }
    var `type` = null
    // Swagger ReflectionUtils can't handle file or array datatype
    if (!"".equalsIgnoreCase(param.dataType) && !"file".equalsIgnoreCase(param.dataType) && !"array".equalsIgnoreCase(param.dataType)) `type` = PlayReader.typeFromString(param.dataType, cls)
    else if (param.dataTypeClass != null && !PlayReader.isVoid(param.dataTypeClass)) `type` = param.dataTypeClass
    val result = ParameterProcessor.applyAnnotations(getSwagger, p, if (`type` == null) classOf[String]
    else `type`, Collections.singletonList(param))
    if (result.isInstanceOf[AbstractSerializableParameter[_]] && `type` != null) {
      val schema = createProperty(`type`)
      p.asInstanceOf[AbstractSerializableParameter[_]].setProperty(schema)
    }
    result
  }

  private def parseMethod(cls: Class[_], method: Method, route: Route): Operation = {
    val operation = new Operation
    val apiOperation = ReflectionUtils.getAnnotation(method, classOf[ApiOperation])
    val responseAnnotation = ReflectionUtils.getAnnotation(method, classOf[ApiResponses])
    var operationId = method.getName
    operation.operationId(operationId)
    var responseContainer = null
    var responseType = null
    var defaultResponseHeaders = new util.HashMap[String, Property]
    if (apiOperation != null) {
      if (apiOperation.hidden) return null
      if (!("" == apiOperation.nickname)) operationId = apiOperation.nickname
      defaultResponseHeaders = parseResponseHeaders(apiOperation.responseHeaders)
      operation.summary(apiOperation.value).description(apiOperation.notes)
      if (apiOperation.response != null && !PlayReader.isVoid(apiOperation.response)) responseType = apiOperation.response
      if (!("" == apiOperation.responseContainer)) responseContainer = apiOperation.responseContainer
      if (apiOperation.authorizations != null) {
        val securities = new util.ArrayList[SecurityRequirement]
        for (auth <- apiOperation.authorizations) {
          if (auth.value != null && !("" == auth.value)) {
            val security = new SecurityRequirement
            security.setName(auth.value)
            val scopes = auth.scopes
            for (scope <- scopes) {
              if (scope.scope != null && !("" == scope.scope)) security.addScope(scope.scope)
            }
            securities.add(security)
          }
        }
        if (securities.size > 0) securities.forEach(operation.security)
      }
      if (apiOperation.consumes != null && !apiOperation.consumes.isEmpty) operation.consumes(util.Arrays.asList(toArray(apiOperation.consumes)))
      if (apiOperation.produces != null && !apiOperation.produces.isEmpty) operation.produces(util.Arrays.asList(toArray(apiOperation.produces)))
    }
    if (apiOperation != null && StringUtils.isNotEmpty(apiOperation.responseReference)) {
      val response = new Response().description(PlayReader.SUCCESSFUL_OPERATION)
      response.schema(new RefProperty(apiOperation.responseReference))
      operation.addResponse(String.valueOf(apiOperation.code), response)
    }
    else if (responseType == null) { // pick out response from method declaration
      responseType = method.getGenericReturnType
    }
    if (PlayReader.isValidResponse(responseType)) {
      val property = ModelConverters.getInstance.readAsProperty(responseType)
      if (property != null) {
        val responseProperty = PlayReader.ContainerWrapper.wrapContainer(responseContainer, property)
        val responseCode = if (apiOperation == null) 200
        else apiOperation.code
        operation.response(responseCode, new Response().description(PlayReader.SUCCESSFUL_OPERATION).schema(responseProperty).headers(defaultResponseHeaders))
        appendModels(responseType)
      }
    }
    operation.operationId(operationId)
    if (responseAnnotation != null) for (apiResponse <- responseAnnotation.value) {
      val responseHeaders = parseResponseHeaders(apiResponse.responseHeaders)
      val response = new Response().description(apiResponse.message).headers(responseHeaders)
      if (apiResponse.code == 0) operation.defaultResponse(response)
      else operation.response(apiResponse.code, response)
      if (StringUtils.isNotEmpty(apiResponse.reference)) response.schema(new RefProperty(apiResponse.reference))
      else if (!PlayReader.isVoid(apiResponse.response)) {
        responseType = apiResponse.response
        val property = ModelConverters.getInstance.readAsProperty(responseType)
        if (property != null) {
          response.schema(PlayReader.ContainerWrapper.wrapContainer(apiResponse.responseContainer, property))
          appendModels(responseType)
        }
      }
    }
    if (ReflectionUtils.getAnnotation(method, classOf[Deprecated]) != null) operation.setDeprecated(true)
    val parameters = getParameters(cls, method, route)
    parameters.forEach(operation.parameter)
    if (operation.getResponses == null) {
      val response = new Response().description(PlayReader.SUCCESSFUL_OPERATION)
      operation.defaultResponse(response)
    }
    operation
  }

  private def getParamType(cls: Class[_], method: Method, simpleTypeName: String, position: Int): Type = try {
    val `type` = PlayReader.getOptionTypeFromString(simpleTypeName, cls)
    if (`type` != null) return `type`
    val genericParameterTypes = method.getGenericParameterTypes
    Json.mapper.getTypeFactory.constructType(genericParameterTypes(position), cls)
  } catch {
    case e: Exception =>
      Logger.of("swagger").error(String.format("Exception getting parameter type for method %s, param %s at position %d", method, simpleTypeName, position), e)
      null
  }

  private def getParamAnnotations(cls: Class[_], genericParameterTypes: Array[Type], paramAnnotations: Array[Array[Annotation]], simpleTypeName: String, fieldPosition: Int) = try
    java.util.Arrays.asList(paramAnnotations(fieldPosition))
  catch {
    case e: Exception =>
      Logger.of("swagger").error(String.format("Exception getting parameter type for %s at position %d", simpleTypeName, fieldPosition), e)
      null
  }

  private def getParamAnnotations(cls: Class[_], method: Method, simpleTypeName: String, fieldPosition: Int): util.List[Annotation] = {
    val genericParameterTypes = method.getGenericParameterTypes
    val paramAnnotations = method.getParameterAnnotations
    var annotations = getParamAnnotations(cls, genericParameterTypes, paramAnnotations, simpleTypeName, fieldPosition)
    if (annotations != null) return annotations
    // Fallback to type
    var i = 0
    while ( {
      i < genericParameterTypes.length
    }) {
      annotations = getParamAnnotations(cls, genericParameterTypes, paramAnnotations, simpleTypeName, i)
      if (annotations != null) return annotations

      {
        i += 1; i - 1
      }
    }
    null
  }

  protected def extractTags(api: Api) = {
    val output = new java.util.LinkedHashSet[String]
    var hasExplicitTags = false
    for (tag <- api.tags) {
      if (!("" == tag)) {
        hasExplicitTags = true
        output.add(tag)
      }
    }
    if (!hasExplicitTags) { // derive tag from api path + description
      val tagString = api.value.replace("/", "")
      if (!("" == tagString)) output.add(tagString)
    }
    output
  }

  private def getParameters(cls: Class[_], method: Method, route: Route): java.util.List[Parameter] = { // TODO now consider only parameters defined in route, excluding body parameters
    // understand how to possibly infer body/form params e.g. from @BodyParser or
    // other annotation
    val parameters = new java.util.ArrayList[Parameter]
    if (!route.call.parameters.isDefined) return parameters
    val iter = route.call.parameters.get.iterator
    var fieldPosition = 0
    while ( {
      iter.hasNext
    }) {
      val p = iter.next
      if (!p.fixed.isEmpty) {
        continue //todo: continue is not supported}
        var parameter = null
        var `def` = CrossUtil.getParameterDefaultField(p)
        if (`def`.startsWith("\"") && `def`.endsWith("\"")) `def` = `def`.substring(1, `def`.length - 1)
        val `type` = getParamType(cls, method, p.typeName, fieldPosition)
        val schema = createProperty(`type`)
        if (route.path.has(p.name)) { // it's a path param
          parameter = new PathParameter
          parameter.asInstanceOf[PathParameter].setDefaultValue(`def`)
          if (schema != null) parameter.asInstanceOf[PathParameter].setProperty(schema)
        }
        else { // it's a query string param
          parameter = new QueryParameter
          parameter.asInstanceOf[QueryParameter].setDefaultValue(`def`)
          if (schema != null) parameter.asInstanceOf[QueryParameter].setProperty(schema)
        }
        parameter.setName(p.name)
        val annotations = getParamAnnotations(cls, method, p.typeName, fieldPosition)
        ParameterProcessor.applyAnnotations(getSwagger, parameter, `type`, annotations)
        parameters.add(parameter)
        fieldPosition += 1
      }
      return parameters
    }
  }


  private def createProperty(`type`: Type)= {
    return enforcePrimitive(ModelConverters.getInstance.readAsProperty(`type`), 0)
  }

    private def enforcePrimitive(in: Property, level: Int) = {
      if (in.isInstanceOf[RefProperty]) return new StringProperty
      if (in.isInstanceOf[ArrayProperty]) if (level == 0) {
        val array = in.asInstanceOf[ArrayProperty]
        array.setItems(enforcePrimitive(array.getItems, level + 1))
      }
      else return new StringProperty
      return in
    }

    private def appendModels(`type`: Type): Unit

    =
    {
      val models = ModelConverters.getInstance.readAll(`type`)
      import scala.collection.JavaConversions._
      for (entry <- models.entrySet) {
        getSwagger.model(entry.getKey, entry.getValue)
      }
    }

    private def parseResponseHeaders(headers: Array[ResponseHeader])

    =
    {
      var responseHeaders = null
      if (headers != null && headers.length > 0) for (header <- headers) {
        val name = header.name
        if (!("" == name)) {
          if (responseHeaders == null) responseHeaders = new util.HashMap[String, Property]
          val description = header.description
          val cls = header.response
          if (!PlayReader.isVoid(cls)) {
            val property = ModelConverters.getInstance.readAsProperty(cls)
            if (property != null) {
              val responseProperty = PlayReader.ContainerWrapper.wrapContainer(header.responseContainer, property, PlayReader.ContainerWrapper.ARRAY, PlayReader.ContainerWrapper.LIST, PlayReader.ContainerWrapper.SET)
              responseProperty.setDescription(description)
              responseHeaders.put(name, responseProperty)
              appendModels(cls)
            }
          }
        }
      }
      return responseHeaders
    }

    def getFullMethodName(clazz: Class[_], method: Method) = if (!clazz.getCanonicalName.contains("$")) clazz.getCanonicalName + "$." + method.getName
    else clazz.getCanonicalName + "." + method.getName

    def extractOperationMethod(apiOperation: ApiOperation, method: Method, route: Route) = {
      var httpMethod = null
      if (route != null) try
        httpMethod = route.verb.toString.toLowerCase
      catch {
        case e: Exception =>
          Logger.of("swagger").error("http method not found for method: " + method.getName, e)
      }
      if (httpMethod == null) if (!StringUtils.isEmpty(apiOperation.httpMethod)) httpMethod = apiOperation.httpMethod
      httpMethod
    }

    private def toArray(csString: String)

    =
    {
      if (StringUtils.isEmpty(csString)) return Array[String](csString)
      var i = 0
      val result = csString.split(",")
      for (c <- result) {
        result(i) = c.trim
        i += 1
      }
      result
    }
  }
*/
