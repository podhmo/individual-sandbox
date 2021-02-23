
## Attachment

An object representing a container instance or task attachment.

| name | type | description |
| :--- | :--- | :--- |
| details | list[[KeyValuePair](#KeyValuePair)]? | |
| id | str? | |
| status | str? | |
| type | str? | |

## KeyValuePair

A key-value pair object.

| name | type | description |
| :--- | :--- | :--- |
| name | str? | |
| value | str? | |

## AttachmentStateChange

An object representing a change in state for a task attachment.

| name | type | description |
| :--- | :--- | :--- |
| attachmentArn | str | |
| status | str | |

## Attribute

An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| name | str | |
| targetId | str? | |
| targetType |  `container-instance` ? | |
| value | str? | |

## AutoScalingGroupProvider

The details of the Auto Scaling group for the capacity provider.

| name | type | description |
| :--- | :--- | :--- |
| autoScalingGroupArn | str | |
| managedScaling | [ManagedScaling](#ManagedScaling)? | |
| managedTerminationProtection |  `ENABLED, DISABLED` ? | |

## ManagedScaling

<p>The managed scaling settings for the Auto Scaling group capacity provider.</p> <p>When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified <code>targetCapacity</code> value as the target value for the metric. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling">Using Managed Scaling</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.</p>

| name | type | description |
| :--- | :--- | :--- |
| instanceWarmupPeriod | int? | |
| maximumScalingStepSize | int? | |
| minimumScalingStepSize | int? | |
| status |  `ENABLED, DISABLED` ? | |
| targetCapacity | int? | |

## AutoScalingGroupProviderUpdate

The details of the Auto Scaling group capacity provider to update.

| name | type | description |
| :--- | :--- | :--- |
| managedScaling | [ManagedScaling](#ManagedScaling)? | |
| managedTerminationProtection |  `ENABLED, DISABLED` ? | |

## AwsVpcConfiguration

An object representing the networking details for a task or service.

| name | type | description |
| :--- | :--- | :--- |
| assignPublicIp |  `ENABLED, DISABLED` ? | |
| securityGroups | list[str]? | |
| subnets | list[str] | |

## CapacityProvider

The details of a capacity provider.

| name | type | description |
| :--- | :--- | :--- |
| autoScalingGroupProvider | [AutoScalingGroupProvider](#AutoScalingGroupProvider)? | |
| capacityProviderArn | str? | |
| name | str? | |
| status |  `ACTIVE, INACTIVE` ? | |
| tags | list[[Tag](#Tag)]? | |
| updateStatus |  `DELETE_IN_PROGRESS, DELETE_COMPLETE, DELETE_FAILED, UPDATE_IN_PROGRESS, UPDATE_COMPLETE, UPDATE_FAILED` ? | |
| updateStatusReason | str? | |

## Tag

<p>The metadata that you apply to a resource to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.</p> <p>The following basic restrictions apply to tags:</p> <ul> <li> <p>Maximum number of tags per resource - 50</p> </li> <li> <p>For each resource, each tag key must be unique, and each tag key can have only one value.</p> </li> <li> <p>Maximum key length - 128 Unicode characters in UTF-8</p> </li> <li> <p>Maximum value length - 256 Unicode characters in UTF-8</p> </li> <li> <p>If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.</p> </li> <li> <p>Tag keys and values are case-sensitive.</p> </li> <li> <p>Do not use <code>aws:</code>, <code>AWS:</code>, or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.</p> </li> </ul>

| name | type | description |
| :--- | :--- | :--- |
| key | str? | |
| value | str? | |

## CapacityProviderStrategyItem

The details of a capacity provider strategy.

| name | type | description |
| :--- | :--- | :--- |
| base | int? | |
| capacityProvider | str | |
| weight | int? | |

## Cluster

A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.

| name | type | description |
| :--- | :--- | :--- |
| activeServicesCount | int? | |
| attachments | list[[Attachment](#Attachment)]? | |
| attachmentsStatus | str? | |
| capacityProviders | list[str]? | |
| clusterArn | str? | |
| clusterName | str? | |
| defaultCapacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| pendingTasksCount | int? | |
| registeredContainerInstancesCount | int? | |
| runningTasksCount | int? | |
| settings | list[[ClusterSetting](#ClusterSetting)]? | |
| statistics | list[[KeyValuePair](#KeyValuePair)]? | |
| status | str? | |
| tags | list[[Tag](#Tag)]? | |

## ClusterSetting

The settings to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster.

| name | type | description |
| :--- | :--- | :--- |
| name |  `containerInsights` ? | |
| value | str? | |

## Container

A Docker container that is part of a task.

| name | type | description |
| :--- | :--- | :--- |
| containerArn | str? | |
| cpu | str? | |
| exitCode | int? | |
| gpuIds | list[str]? | |
| healthStatus |  `HEALTHY, UNHEALTHY, UNKNOWN` ? | |
| image | str? | |
| imageDigest | str? | |
| lastStatus | str? | |
| memory | str? | |
| memoryReservation | str? | |
| name | str? | |
| networkBindings | list[[NetworkBinding](#NetworkBinding)]? | |
| networkInterfaces | list[[NetworkInterface](#NetworkInterface)]? | |
| reason | str? | |
| runtimeId | str? | |
| taskArn | str? | |

## NetworkInterface

An object representing the elastic network interface for tasks that use the <code>awsvpc</code> network mode.

| name | type | description |
| :--- | :--- | :--- |
| attachmentId | str? | |
| ipv6Address | str? | |
| privateIpv4Address | str? | |

## NetworkBinding

Details on the network bindings between a container and its host container instance. After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.

| name | type | description |
| :--- | :--- | :--- |
| bindIP | str? | |
| containerPort | int? | |
| hostPort | int? | |
| protocol |  `tcp, udp` ? | |

## ContainerDefinition

Container definitions are used in task definitions to describe the different containers that are launched as part of a task.

| name | type | description |
| :--- | :--- | :--- |
| command | list[str]? | |
| cpu | int? | |
| dependsOn | list[[ContainerDependency](#ContainerDependency)]? | |
| disableNetworking | bool? | |
| dnsSearchDomains | list[str]? | |
| dnsServers | list[str]? | |
| dockerLabels | dict[str, str]? | |
| dockerSecurityOptions | list[str]? | |
| entryPoint | list[str]? | |
| environment | list[[KeyValuePair](#KeyValuePair)]? | |
| environmentFiles | list[[EnvironmentFile](#EnvironmentFile)]? | |
| essential | bool? | |
| extraHosts | list[[HostEntry](#HostEntry)]? | |
| firelensConfiguration | [FirelensConfiguration](#FirelensConfiguration)? | |
| healthCheck | [HealthCheck](#HealthCheck)? | |
| hostname | str? | |
| image | str? | |
| interactive | bool? | |
| links | list[str]? | |
| linuxParameters | [LinuxParameters](#LinuxParameters)? | |
| logConfiguration | [LogConfiguration](#LogConfiguration)? | |
| memory | int? | |
| memoryReservation | int? | |
| mountPoints | list[[MountPoint](#MountPoint)]? | |
| name | str? | |
| portMappings | list[[PortMapping](#PortMapping)]? | |
| privileged | bool? | |
| pseudoTerminal | bool? | |
| readonlyRootFilesystem | bool? | |
| repositoryCredentials | [RepositoryCredentials](#RepositoryCredentials)? | |
| resourceRequirements | list[[ResourceRequirement](#ResourceRequirement)]? | |
| secrets | list[[Secret](#Secret)]? | |
| startTimeout | int? | |
| stopTimeout | int? | |
| systemControls | list[[SystemControl](#SystemControl)]? | |
| ulimits | list[[Ulimit](#Ulimit)]? | |
| user | str? | |
| volumesFrom | list[[VolumeFrom](#VolumeFrom)]? | |
| workingDirectory | str? | |

## VolumeFrom

Details on a data volume from another container in the same task definition.

| name | type | description |
| :--- | :--- | :--- |
| readOnly | bool? | |
| sourceContainer | str? | |

## Ulimit

The <code>ulimit</code> settings to pass to the container.

| name | type | description |
| :--- | :--- | :--- |
| hardLimit | int | |
| name |  `core, cpu, data, fsize, locks, memlock, msgqueue, nice, nofile, nproc, rss, rtprio, rttime, sigpending, stack`  | |
| softLimit | int | |

## SystemControl

<p>A list of namespaced kernel parameters to set in the container. This parameter maps to <code>Sysctls</code> in the <a href="https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate">Create a container</a> section of the <a href="https://docs.docker.com/engine/api/v1.35/">Docker Remote API</a> and the <code>--sysctl</code> option to <a href="https://docs.docker.com/engine/reference/run/#security-configuration">docker run</a>.</p> <p>It is not recommended that you specify network-related <code>systemControls</code> parameters for multiple containers in a single task that also uses either the <code>awsvpc</code> or <code>host</code> network mode for the following reasons:</p> <ul> <li> <p>For tasks that use the <code>awsvpc</code> network mode, if you set <code>systemControls</code> for any container, it applies to all containers in the task. If you set different <code>systemControls</code> for multiple containers in a single task, the container that is started last determines which <code>systemControls</code> take effect.</p> </li> <li> <p>For tasks that use the <code>host</code> network mode, the <code>systemControls</code> parameter applies to the container instance's kernel parameter as well as that of all containers of any tasks running on that container instance.</p> </li> </ul>

| name | type | description |
| :--- | :--- | :--- |
| namespace | str? | |
| value | str? | |

## Secret

<p>An object representing the secret to expose to your container. Secrets can be exposed to a container in the following ways:</p> <ul> <li> <p>To inject sensitive data into your containers as environment variables, use the <code>secrets</code> container definition parameter.</p> </li> <li> <p>To reference sensitive information in the log configuration of a container, use the <code>secretOptions</code> container definition parameter.</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html">Specifying Sensitive Data</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

| name | type | description |
| :--- | :--- | :--- |
| name | str | |
| valueFrom | str | |

## ResourceRequirement

The type and amount of a resource to assign to a container. The supported resource types are GPUs and Elastic Inference accelerators. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html">Working with GPUs on Amazon ECS</a> or <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html">Working with Amazon Elastic Inference on Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i> 

| name | type | description |
| :--- | :--- | :--- |
| type |  `GPU, InferenceAccelerator`  | |
| value | str | |

## RepositoryCredentials

The repository credentials for private registry authentication.

| name | type | description |
| :--- | :--- | :--- |
| credentialsParameter | str | |

## PortMapping

<p>Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.</p> <p>If you are using containers in a task with the <code>awsvpc</code> or <code>host</code> network mode, exposed ports should be specified using <code>containerPort</code>. The <code>hostPort</code> can be left blank or it must be the same value as the <code>containerPort</code>.</p> <p>After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>

| name | type | description |
| :--- | :--- | :--- |
| containerPort | int? | |
| hostPort | int? | |
| protocol |  `tcp, udp` ? | |

## MountPoint

Details on a volume mount point that is used in a container definition.

| name | type | description |
| :--- | :--- | :--- |
| containerPath | str? | |
| readOnly | bool? | |
| sourceVolume | str? | |

## LogConfiguration

<p>The log configuration for the container. This parameter maps to <code>LogConfig</code> in the <a href="https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate">Create a container</a> section of the <a href="https://docs.docker.com/engine/api/v1.35/">Docker Remote API</a> and the <code>--log-driver</code> option to <a href="https://docs.docker.com/engine/reference/commandline/run/"> <code>docker run</code> </a>.</p> <p>By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver configuration in the container definition. For more information on the options for different supported log drivers, see <a href="https://docs.docker.com/engine/admin/logging/overview/">Configure logging drivers</a> in the Docker documentation.</p> <p>The following should be noted when specifying a log configuration for your containers:</p> <ul> <li> <p>Amazon ECS currently supports a subset of the logging drivers available to the Docker daemon (shown in the valid values below). Additional log drivers may be available in future releases of the Amazon ECS container agent.</p> </li> <li> <p>This parameter requires version 1.18 of the Docker Remote API or greater on your container instance.</p> </li> <li> <p>For tasks hosted on Amazon EC2 instances, the Amazon ECS container agent must register the available logging drivers with the <code>ECS_AVAILABLE_LOGGING_DRIVERS</code> environment variable before containers placed on that instance can use these log configuration options. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html">Amazon ECS container agent configuration</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </li> <li> <p>For tasks on AWS Fargate, because you do not have access to the underlying infrastructure your tasks are hosted on, any additional software needed will have to be installed outside of the task. For example, the Fluentd output aggregators or a remote host running Logstash to send Gelf logs to.</p> </li> </ul>

| name | type | description |
| :--- | :--- | :--- |
| logDriver |  `json-file, syslog, journald, gelf, fluentd, awslogs, splunk, awsfirelens`  | |
| options | dict[str, str]? | |
| secretOptions | list[[Secret](#Secret)]? | |

## LinuxParameters

Linux-specific options that are applied to the container, such as Linux <a>KernelCapabilities</a>.

| name | type | description |
| :--- | :--- | :--- |
| capabilities | [KernelCapabilities](#KernelCapabilities)? | |
| devices | list[[Device](#Device)]? | |
| initProcessEnabled | bool? | |
| maxSwap | int? | |
| sharedMemorySize | int? | |
| swappiness | int? | |
| tmpfs | list[[Tmpfs](#Tmpfs)]? | |

## Tmpfs

The container path, mount options, and size of the tmpfs mount.

| name | type | description |
| :--- | :--- | :--- |
| containerPath | str | |
| mountOptions | list[str]? | |
| size | int | |

## Device

An object representing a container instance host device.

| name | type | description |
| :--- | :--- | :--- |
| containerPath | str? | |
| hostPath | str | |
| permissions | list[ `read, write, mknod` ]? | |

## KernelCapabilities

The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <a href="https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities">Runtime privilege and Linux capabilities</a> in the <i>Docker run reference</i>. For more detailed information on these Linux capabilities, see the <a href="http://man7.org/linux/man-pages/man7/capabilities.7.html">capabilities(7)</a> Linux manual page.

| name | type | description |
| :--- | :--- | :--- |
| add | list[str]? | |
| drop | list[str]? | |

## HealthCheck

<p>An object representing a container health check. Health check parameters that are specified in a container definition override any Docker health checks that exist in the container image (such as those specified in a parent image or from the image's Dockerfile).</p> <p>You can view the health status of both individual containers and a task with the DescribeTasks API operation or when viewing the task details in the console.</p> <p>The following describes the possible <code>healthStatus</code> values for a container:</p> <ul> <li> <p> <code>HEALTHY</code>-The container health check has passed successfully.</p> </li> <li> <p> <code>UNHEALTHY</code>-The container health check has failed.</p> </li> <li> <p> <code>UNKNOWN</code>-The container health check is being evaluated or there is no container health check defined.</p> </li> </ul> <p>The following describes the possible <code>healthStatus</code> values for a task. The container health check status of nonessential containers do not have an effect on the health status of a task.</p> <ul> <li> <p> <code>HEALTHY</code>-All essential containers within the task have passed their health checks.</p> </li> <li> <p> <code>UNHEALTHY</code>-One or more essential containers have failed their health check.</p> </li> <li> <p> <code>UNKNOWN</code>-The essential containers within the task are still having their health checks evaluated or there are no container health checks defined.</p> </li> </ul> <p>If a task is run manually, and not as part of a service, the task will continue its lifecycle regardless of its health status. For tasks that are part of a service, if the task reports as unhealthy then the task will be stopped and the service scheduler will replace it.</p> <p>The following are notes about container health check support:</p> <ul> <li> <p>Container health checks require version 1.17.0 or greater of the Amazon ECS container agent. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html">Updating the Amazon ECS Container Agent</a>.</p> </li> <li> <p>Container health checks are supported for Fargate tasks if you are using platform version 1.1.0 or greater. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html">AWS Fargate Platform Versions</a>.</p> </li> <li> <p>Container health checks are not supported for tasks that are part of a service that is configured to use a Classic Load Balancer.</p> </li> </ul>

| name | type | description |
| :--- | :--- | :--- |
| command | list[str] | |
| interval | int? | |
| retries | int? | |
| startPeriod | int? | |
| timeout | int? | |

## FirelensConfiguration

The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html">Custom Log Routing</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| options | dict[str, str]? | |
| type |  `fluentd, fluentbit`  | |

## HostEntry

Hostnames and IP address entries that are added to the <code>/etc/hosts</code> file of a container via the <code>extraHosts</code> parameter of its <a>ContainerDefinition</a>. 

| name | type | description |
| :--- | :--- | :--- |
| hostname | str | |
| ipAddress | str | |

## EnvironmentFile

<p>A list of files containing the environment variables to pass to a container. You can specify up to ten environment files. The file must have a <code>.env</code> file extension. Each line in an environment file should contain an environment variable in <code>VARIABLE=VALUE</code> format. Lines beginning with <code>#</code> are treated as comments and are ignored. For more information on the environment variable file syntax, see <a href="https://docs.docker.com/compose/env-file/">Declare default environment variables in file</a>.</p> <p>If there are environment variables specified using the <code>environment</code> parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html">Specifying Environment Variables</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>This field is not valid for containers in tasks using the Fargate launch type.</p>

| name | type | description |
| :--- | :--- | :--- |
| type |  `s3`  | |
| value | str | |

## ContainerDependency

<p>The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.</p> <p>Your Amazon ECS container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html">Updating the Amazon ECS Container Agent</a> in the <i>Amazon Elastic Container Service Developer Guide</i>. If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the <code>ecs-init</code> package. If your container instances are launched from version <code>20190301</code> or later, then they contain the required versions of the container agent and <code>ecs-init</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html">Amazon ECS-optimized Linux AMI</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <note> <p>For tasks using the Fargate launch type, this parameter requires that the task or service uses platform version 1.3.0 or later.</p> </note>

| name | type | description |
| :--- | :--- | :--- |
| condition |  `START, COMPLETE, SUCCESS, HEALTHY`  | |
| containerName | str | |

## ContainerInstance

An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.

| name | type | description |
| :--- | :--- | :--- |
| agentConnected | bool? | |
| agentUpdateStatus |  `PENDING, STAGING, STAGED, UPDATING, UPDATED, FAILED` ? | |
| attachments | list[[Attachment](#Attachment)]? | |
| attributes | list[[Attribute](#Attribute)]? | |
| capacityProviderName | str? | |
| containerInstanceArn | str? | |
| ec2InstanceId | str? | |
| pendingTasksCount | int? | |
| registeredAt | [datetime](#datetime)? | |
| registeredResources | list[[Resource](#Resource)]? | |
| remainingResources | list[[Resource](#Resource)]? | |
| runningTasksCount | int? | |
| status | str? | |
| statusReason | str? | |
| tags | list[[Tag](#Tag)]? | |
| version | int? | |
| versionInfo | [VersionInfo](#VersionInfo)? | |

## VersionInfo

The Docker and Amazon ECS container agent version information about a container instance.

| name | type | description |
| :--- | :--- | :--- |
| agentHash | str? | |
| agentVersion | str? | |
| dockerVersion | str? | |

## Resource

Describes the resources available for a container instance.

| name | type | description |
| :--- | :--- | :--- |
| doubleValue | float? | |
| integerValue | int? | |
| longValue | int? | |
| name | str? | |
| stringSetValue | list[str]? | |
| type | str? | |

## ContainerOverride

The overrides that should be sent to a container. An empty container override can be passed in. An example of an empty container override would be <code>{"containerOverrides": [ ] }</code>. If a non-empty container override is specified, the <code>name</code> parameter must be included.

| name | type | description |
| :--- | :--- | :--- |
| command | list[str]? | |
| cpu | int? | |
| environment | list[[KeyValuePair](#KeyValuePair)]? | |
| environmentFiles | list[[EnvironmentFile](#EnvironmentFile)]? | |
| memory | int? | |
| memoryReservation | int? | |
| name | str? | |
| resourceRequirements | list[[ResourceRequirement](#ResourceRequirement)]? | |

## ContainerStateChange

An object representing a change in state for a container.

| name | type | description |
| :--- | :--- | :--- |
| containerName | str? | |
| exitCode | int? | |
| imageDigest | str? | |
| networkBindings | list[[NetworkBinding](#NetworkBinding)]? | |
| reason | str? | |
| runtimeId | str? | |
| status | str? | |

## CreateCapacityProviderRequest



| name | type | description |
| :--- | :--- | :--- |
| autoScalingGroupProvider | [AutoScalingGroupProvider](#AutoScalingGroupProvider) | |
| name | str | |
| tags | list[[Tag](#Tag)]? | |

## CreateCapacityProviderResponse



| name | type | description |
| :--- | :--- | :--- |
| capacityProvider | [CapacityProvider](#CapacityProvider)? | |

## CreateClusterRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviders | list[str]? | |
| clusterName | str? | |
| defaultCapacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| settings | list[[ClusterSetting](#ClusterSetting)]? | |
| tags | list[[Tag](#Tag)]? | |

## CreateClusterResponse



| name | type | description |
| :--- | :--- | :--- |
| cluster | [Cluster](#Cluster)? | |

## CreateServiceRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| clientToken | str? | |
| cluster | str? | |
| deploymentConfiguration | [DeploymentConfiguration](#DeploymentConfiguration)? | |
| deploymentController | [DeploymentController](#DeploymentController)? | |
| desiredCount | int? | |
| enableECSManagedTags | bool? | |
| healthCheckGracePeriodSeconds | int? | |
| launchType |  `EC2, FARGATE` ? | |
| loadBalancers | list[[LoadBalancer](#LoadBalancer)]? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| placementConstraints | list[[PlacementConstraint](#PlacementConstraint)]? | |
| placementStrategy | list[[PlacementStrategy](#PlacementStrategy)]? | |
| platformVersion | str? | |
| propagateTags |  `TASK_DEFINITION, SERVICE` ? | |
| role | str? | |
| schedulingStrategy |  `REPLICA, DAEMON` ? | |
| serviceName | str | |
| serviceRegistries | list[[ServiceRegistry](#ServiceRegistry)]? | |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | str? | |

## ServiceRegistry

Details of the service registry.

| name | type | description |
| :--- | :--- | :--- |
| containerName | str? | |
| containerPort | int? | |
| port | int? | |
| registryArn | str? | |

## PlacementStrategy

The task placement strategy for a task or service. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html">Task Placement Strategies</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| field | str? | |
| type |  `random, spread, binpack` ? | |

## PlacementConstraint

<p>An object representing a constraint on task placement. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <note> <p>If you are using the Fargate launch type, task placement constraints are not supported.</p> </note>

| name | type | description |
| :--- | :--- | :--- |
| expression | str? | |
| type |  `distinctInstance, memberOf` ? | |

## NetworkConfiguration

An object representing the network configuration for a task or service.

| name | type | description |
| :--- | :--- | :--- |
| awsvpcConfiguration | [AwsVpcConfiguration](#AwsVpcConfiguration)? | |

## LoadBalancer

<p>The load balancer configuration to use with a service or task set.</p> <p>For specific notes and restrictions regarding the use of load balancers with services and task sets, see the CreateService and CreateTaskSet actions.</p>

| name | type | description |
| :--- | :--- | :--- |
| containerName | str? | |
| containerPort | int? | |
| loadBalancerName | str? | |
| targetGroupArn | str? | |

## DeploymentController

The deployment controller to use for the service. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html">Amazon ECS Deployment Types</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| type |  `ECS, CODE_DEPLOY, EXTERNAL`  | |

## DeploymentConfiguration

Optional deployment parameters that control how many tasks run during a deployment and the ordering of stopping and starting tasks.

| name | type | description |
| :--- | :--- | :--- |
| deploymentCircuitBreaker | [DeploymentCircuitBreaker](#DeploymentCircuitBreaker)? | |
| maximumPercent | int? | |
| minimumHealthyPercent | int? | |

## DeploymentCircuitBreaker

<note> <p>The deployment circuit breaker can only be used for services using the rolling update (<code>ECS</code>) deployment type that are not behind a Classic Load Balancer.</p> </note> <p>The <b>deployment circuit breaker</b> determines whether a service deployment will fail if the service can't reach a steady state. If enabled, a service deployment will transition to a failed state and stop launching new tasks. You can also enable Amazon ECS to roll back your service to the last completed deployment after a failure. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html">Rolling update</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

| name | type | description |
| :--- | :--- | :--- |
| enable | bool | |
| rollback | bool | |

## CreateServiceResponse



| name | type | description |
| :--- | :--- | :--- |
| service | [Service](#Service)? | |

## Service

Details on a service within a cluster

| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| clusterArn | str? | |
| createdAt | [datetime](#datetime)? | |
| createdBy | str? | |
| deploymentConfiguration | [DeploymentConfiguration](#DeploymentConfiguration)? | |
| deploymentController | [DeploymentController](#DeploymentController)? | |
| deployments | list[[Deployment](#Deployment)]? | |
| desiredCount | int? | |
| enableECSManagedTags | bool? | |
| events | list[[ServiceEvent](#ServiceEvent)]? | |
| healthCheckGracePeriodSeconds | int? | |
| launchType |  `EC2, FARGATE` ? | |
| loadBalancers | list[[LoadBalancer](#LoadBalancer)]? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| pendingCount | int? | |
| placementConstraints | list[[PlacementConstraint](#PlacementConstraint)]? | |
| placementStrategy | list[[PlacementStrategy](#PlacementStrategy)]? | |
| platformVersion | str? | |
| propagateTags |  `TASK_DEFINITION, SERVICE` ? | |
| roleArn | str? | |
| runningCount | int? | |
| schedulingStrategy |  `REPLICA, DAEMON` ? | |
| serviceArn | str? | |
| serviceName | str? | |
| serviceRegistries | list[[ServiceRegistry](#ServiceRegistry)]? | |
| status | str? | |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | str? | |
| taskSets | list[[TaskSet](#TaskSet)]? | |

## TaskSet

Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an <code>EXTERNAL</code> deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.

| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| clusterArn | str? | |
| computedDesiredCount | int? | |
| createdAt | [datetime](#datetime)? | |
| externalId | str? | |
| id | str? | |
| launchType |  `EC2, FARGATE` ? | |
| loadBalancers | list[[LoadBalancer](#LoadBalancer)]? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| pendingCount | int? | |
| platformVersion | str? | |
| runningCount | int? | |
| scale | [Scale](#Scale)? | |
| serviceArn | str? | |
| serviceRegistries | list[[ServiceRegistry](#ServiceRegistry)]? | |
| stabilityStatus |  `STEADY_STATE, STABILIZING` ? | |
| stabilityStatusAt | [datetime](#datetime)? | |
| startedBy | str? | |
| status | str? | |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | str? | |
| taskSetArn | str? | |
| updatedAt | [datetime](#datetime)? | |

## Scale

A floating-point percentage of the desired number of tasks to place and keep running in the task set.

| name | type | description |
| :--- | :--- | :--- |
| unit |  `PERCENT` ? | |
| value | float? | |

## ServiceEvent

Details on an event associated with a service.

| name | type | description |
| :--- | :--- | :--- |
| createdAt | [datetime](#datetime)? | |
| id | str? | |
| message | str? | |

## Deployment

The details of an Amazon ECS service deployment. This is used only when a service uses the <code>ECS</code> deployment controller type.

| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| createdAt | [datetime](#datetime)? | |
| desiredCount | int? | |
| failedTasks | int? | |
| id | str? | |
| launchType |  `EC2, FARGATE` ? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| pendingCount | int? | |
| platformVersion | str? | |
| rolloutState |  `COMPLETED, FAILED, IN_PROGRESS` ? | |
| rolloutStateReason | str? | |
| runningCount | int? | |
| status | str? | |
| taskDefinition | str? | |
| updatedAt | [datetime](#datetime)? | |

## CreateTaskSetRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| clientToken | str? | |
| cluster | str | |
| externalId | str? | |
| launchType |  `EC2, FARGATE` ? | |
| loadBalancers | list[[LoadBalancer](#LoadBalancer)]? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| platformVersion | str? | |
| scale | [Scale](#Scale)? | |
| service | str | |
| serviceRegistries | list[[ServiceRegistry](#ServiceRegistry)]? | |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | str | |

## CreateTaskSetResponse



| name | type | description |
| :--- | :--- | :--- |
| taskSet | [TaskSet](#TaskSet)? | |

## DeleteAccountSettingRequest



| name | type | description |
| :--- | :--- | :--- |
| name |  `serviceLongArnFormat, taskLongArnFormat, containerInstanceLongArnFormat, awsvpcTrunking, containerInsights`  | |
| principalArn | str? | |

## DeleteAccountSettingResponse



| name | type | description |
| :--- | :--- | :--- |
| setting | [Setting](#Setting)? | |

## Setting

The current account setting for a resource.

| name | type | description |
| :--- | :--- | :--- |
| name |  `serviceLongArnFormat, taskLongArnFormat, containerInstanceLongArnFormat, awsvpcTrunking, containerInsights` ? | |
| principalArn | str? | |
| value | str? | |

## DeleteAttributesRequest



| name | type | description |
| :--- | :--- | :--- |
| attributes | list[[Attribute](#Attribute)] | |
| cluster | str? | |

## DeleteAttributesResponse



| name | type | description |
| :--- | :--- | :--- |
| attributes | list[[Attribute](#Attribute)]? | |

## DeleteCapacityProviderRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProvider | str | |

## DeleteCapacityProviderResponse



| name | type | description |
| :--- | :--- | :--- |
| capacityProvider | [CapacityProvider](#CapacityProvider)? | |

## DeleteClusterRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str | |

## DeleteClusterResponse



| name | type | description |
| :--- | :--- | :--- |
| cluster | [Cluster](#Cluster)? | |

## DeleteServiceRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| force | bool? | |
| service | str | |

## DeleteServiceResponse



| name | type | description |
| :--- | :--- | :--- |
| service | [Service](#Service)? | |

## DeleteTaskSetRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str | |
| force | bool? | |
| service | str | |
| taskSet | str | |

## DeleteTaskSetResponse



| name | type | description |
| :--- | :--- | :--- |
| taskSet | [TaskSet](#TaskSet)? | |

## DeregisterContainerInstanceRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstance | str | |
| force | bool? | |

## DeregisterContainerInstanceResponse



| name | type | description |
| :--- | :--- | :--- |
| containerInstance | [ContainerInstance](#ContainerInstance)? | |

## DeregisterTaskDefinitionRequest



| name | type | description |
| :--- | :--- | :--- |
| taskDefinition | str | |

## DeregisterTaskDefinitionResponse



| name | type | description |
| :--- | :--- | :--- |
| taskDefinition | [TaskDefinition](#TaskDefinition)? | |

## TaskDefinition

The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.

| name | type | description |
| :--- | :--- | :--- |
| compatibilities | list[ `EC2, FARGATE` ]? | |
| containerDefinitions | list[[ContainerDefinition](#ContainerDefinition)]? | |
| cpu | str? | |
| deregisteredAt | [datetime](#datetime)? | |
| executionRoleArn | str? | |
| family | str? | |
| inferenceAccelerators | list[[InferenceAccelerator](#InferenceAccelerator)]? | |
| ipcMode |  `host, task, none` ? | |
| memory | str? | |
| networkMode |  `bridge, host, awsvpc, none` ? | |
| pidMode |  `host, task` ? | |
| placementConstraints | list[[TaskDefinitionPlacementConstraint](#TaskDefinitionPlacementConstraint)]? | |
| proxyConfiguration | [ProxyConfiguration](#ProxyConfiguration)? | |
| registeredAt | [datetime](#datetime)? | |
| registeredBy | str? | |
| requiresAttributes | list[[Attribute](#Attribute)]? | |
| requiresCompatibilities | list[ `EC2, FARGATE` ]? | |
| revision | int? | |
| status |  `ACTIVE, INACTIVE` ? | |
| taskDefinitionArn | str? | |
| taskRoleArn | str? | |
| volumes | list[[Volume](#Volume)]? | |

## Volume

A data volume used in a task definition. For tasks that use the Amazon Elastic File System (Amazon EFS), specify an <code>efsVolumeConfiguration</code>. For Windows tasks that use Amazon FSx for Windows File Server file system, specify a <code>fsxWindowsFileServerVolumeConfiguration</code>. For tasks that use a Docker volume, specify a <code>DockerVolumeConfiguration</code>. For tasks that use a bind mount host volume, specify a <code>host</code> and optional <code>sourcePath</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html">Using Data Volumes in Tasks</a>.

| name | type | description |
| :--- | :--- | :--- |
| dockerVolumeConfiguration | [DockerVolumeConfiguration](#DockerVolumeConfiguration)? | |
| efsVolumeConfiguration | [EFSVolumeConfiguration](#EFSVolumeConfiguration)? | |
| fsxWindowsFileServerVolumeConfiguration | [FSxWindowsFileServerVolumeConfiguration](#FSxWindowsFileServerVolumeConfiguration)? | |
| host | [HostVolumeProperties](#HostVolumeProperties)? | |
| name | str? | |

## HostVolumeProperties

Details on a container instance bind mount host volume.

| name | type | description |
| :--- | :--- | :--- |
| sourcePath | str? | |

## FSxWindowsFileServerVolumeConfiguration

<p>This parameter is specified when you are using <a href="https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html">Amazon FSx for Windows File Server</a> file system for task storage.</p> <p>For more information and the input format, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html">Amazon FSx for Windows File Server Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

| name | type | description |
| :--- | :--- | :--- |
| authorizationConfig | [FSxWindowsFileServerAuthorizationConfig](#FSxWindowsFileServerAuthorizationConfig) | |
| fileSystemId | str | |
| rootDirectory | str | |

## FSxWindowsFileServerAuthorizationConfig

<p>The authorization configuration details for Amazon FSx for Windows File Server file system. See <a href="https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html">FSxWindowsFileServerVolumeConfiguration</a> in the <i>Amazon Elastic Container Service API Reference</i>.</p> <p>For more information and the input format, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html">Amazon FSx for Windows File Server Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

| name | type | description |
| :--- | :--- | :--- |
| credentialsParameter | str | |
| domain | str | |

## EFSVolumeConfiguration

This parameter is specified when you are using an Amazon Elastic File System file system for task storage. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html">Amazon EFS Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| authorizationConfig | [EFSAuthorizationConfig](#EFSAuthorizationConfig)? | |
| fileSystemId | str | |
| rootDirectory | str? | |
| transitEncryption |  `ENABLED, DISABLED` ? | |
| transitEncryptionPort | int? | |

## EFSAuthorizationConfig

The authorization configuration details for the Amazon EFS file system.

| name | type | description |
| :--- | :--- | :--- |
| accessPointId | str? | |
| iam |  `ENABLED, DISABLED` ? | |

## DockerVolumeConfiguration

This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the <code>local</code> driver. To use bind mounts, specify a <code>host</code> instead.

| name | type | description |
| :--- | :--- | :--- |
| autoprovision | bool? | |
| driver | str? | |
| driverOpts | dict[str, str]? | |
| labels | dict[str, str]? | |
| scope |  `task, shared` ? | |

## ProxyConfiguration

<p>The configuration details for the App Mesh proxy.</p> <p>For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the <code>ecs-init</code> package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version <code>20190301</code> or later, then they contain the required versions of the container agent and <code>ecs-init</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html">Amazon ECS-optimized Linux AMI</a> </p>

| name | type | description |
| :--- | :--- | :--- |
| containerName | str | |
| properties | list[[KeyValuePair](#KeyValuePair)]? | |
| type |  `APPMESH` ? | |

## TaskDefinitionPlacementConstraint

<p>An object representing a constraint on task placement in the task definition. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <note> <p>If you are using the Fargate launch type, task placement constraints are not supported.</p> </note>

| name | type | description |
| :--- | :--- | :--- |
| expression | str? | |
| type |  `memberOf` ? | |

## InferenceAccelerator

Details on a Elastic Inference accelerator. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html">Working with Amazon Elastic Inference on Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| deviceName | str | |
| deviceType | str | |

## DescribeCapacityProvidersRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviders | list[str]? | |
| include | list[ `TAGS` ]? | |
| maxResults | int? | |
| nextToken | str? | |

## DescribeCapacityProvidersResponse



| name | type | description |
| :--- | :--- | :--- |
| capacityProviders | list[[CapacityProvider](#CapacityProvider)]? | |
| failures | list[[Failure](#Failure)]? | |
| nextToken | str? | |

## Failure

A failed resource. For a list of common causes, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html">API failure reasons</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| arn | str? | |
| detail | str? | |
| reason | str? | |

## DescribeClustersRequest



| name | type | description |
| :--- | :--- | :--- |
| clusters | list[str]? | |
| include | list[ `ATTACHMENTS, SETTINGS, STATISTICS, TAGS` ]? | |

## DescribeClustersResponse



| name | type | description |
| :--- | :--- | :--- |
| clusters | list[[Cluster](#Cluster)]? | |
| failures | list[[Failure](#Failure)]? | |

## DescribeContainerInstancesRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstances | list[str] | |
| include | list[ `TAGS` ]? | |

## DescribeContainerInstancesResponse



| name | type | description |
| :--- | :--- | :--- |
| containerInstances | list[[ContainerInstance](#ContainerInstance)]? | |
| failures | list[[Failure](#Failure)]? | |

## DescribeServicesRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| include | list[ `TAGS` ]? | |
| services | list[str] | |

## DescribeServicesResponse



| name | type | description |
| :--- | :--- | :--- |
| failures | list[[Failure](#Failure)]? | |
| services | list[[Service](#Service)]? | |

## DescribeTaskDefinitionRequest



| name | type | description |
| :--- | :--- | :--- |
| include | list[ `TAGS` ]? | |
| taskDefinition | str | |

## DescribeTaskDefinitionResponse



| name | type | description |
| :--- | :--- | :--- |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | [TaskDefinition](#TaskDefinition)? | |

## DescribeTaskSetsRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str | |
| include | list[ `TAGS` ]? | |
| service | str | |
| taskSets | list[str]? | |

## DescribeTaskSetsResponse



| name | type | description |
| :--- | :--- | :--- |
| failures | list[[Failure](#Failure)]? | |
| taskSets | list[[TaskSet](#TaskSet)]? | |

## DescribeTasksRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| include | list[ `TAGS` ]? | |
| tasks | list[str] | |

## DescribeTasksResponse



| name | type | description |
| :--- | :--- | :--- |
| failures | list[[Failure](#Failure)]? | |
| tasks | list[[Task](#Task)]? | |

## Task

Details on a task in a cluster.

| name | type | description |
| :--- | :--- | :--- |
| attachments | list[[Attachment](#Attachment)]? | |
| attributes | list[[Attribute](#Attribute)]? | |
| availabilityZone | str? | |
| capacityProviderName | str? | |
| clusterArn | str? | |
| connectivity |  `CONNECTED, DISCONNECTED` ? | |
| connectivityAt | [datetime](#datetime)? | |
| containerInstanceArn | str? | |
| containers | list[[Container](#Container)]? | |
| cpu | str? | |
| createdAt | [datetime](#datetime)? | |
| desiredStatus | str? | |
| executionStoppedAt | [datetime](#datetime)? | |
| group | str? | |
| healthStatus |  `HEALTHY, UNHEALTHY, UNKNOWN` ? | |
| inferenceAccelerators | list[[InferenceAccelerator](#InferenceAccelerator)]? | |
| lastStatus | str? | |
| launchType |  `EC2, FARGATE` ? | |
| memory | str? | |
| overrides | [TaskOverride](#TaskOverride)? | |
| platformVersion | str? | |
| pullStartedAt | [datetime](#datetime)? | |
| pullStoppedAt | [datetime](#datetime)? | |
| startedAt | [datetime](#datetime)? | |
| startedBy | str? | |
| stopCode |  `TaskFailedToStart, EssentialContainerExited, UserInitiated` ? | |
| stoppedAt | [datetime](#datetime)? | |
| stoppedReason | str? | |
| stoppingAt | [datetime](#datetime)? | |
| tags | list[[Tag](#Tag)]? | |
| taskArn | str? | |
| taskDefinitionArn | str? | |
| version | int? | |

## TaskOverride

The overrides associated with a task.

| name | type | description |
| :--- | :--- | :--- |
| containerOverrides | list[[ContainerOverride](#ContainerOverride)]? | |
| cpu | str? | |
| executionRoleArn | str? | |
| inferenceAcceleratorOverrides | list[[InferenceAcceleratorOverride](#InferenceAcceleratorOverride)]? | |
| memory | str? | |
| taskRoleArn | str? | |

## InferenceAcceleratorOverride

Details on an Elastic Inference accelerator task override. This parameter is used to override the Elastic Inference accelerator specified in the task definition. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html">Working with Amazon Elastic Inference on Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.

| name | type | description |
| :--- | :--- | :--- |
| deviceName | str? | |
| deviceType | str? | |

## DiscoverPollEndpointRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstance | str? | |

## DiscoverPollEndpointResponse



| name | type | description |
| :--- | :--- | :--- |
| endpoint | str? | |
| telemetryEndpoint | str? | |

## ListAccountSettingsRequest



| name | type | description |
| :--- | :--- | :--- |
| effectiveSettings | bool? | |
| maxResults | int? | |
| name |  `serviceLongArnFormat, taskLongArnFormat, containerInstanceLongArnFormat, awsvpcTrunking, containerInsights` ? | |
| nextToken | str? | |
| principalArn | str? | |
| value | str? | |

## ListAccountSettingsResponse



| name | type | description |
| :--- | :--- | :--- |
| nextToken | str? | |
| settings | list[[Setting](#Setting)]? | |

## ListAttributesRequest



| name | type | description |
| :--- | :--- | :--- |
| attributeName | str? | |
| attributeValue | str? | |
| cluster | str? | |
| maxResults | int? | |
| nextToken | str? | |
| targetType |  `container-instance`  | |

## ListAttributesResponse



| name | type | description |
| :--- | :--- | :--- |
| attributes | list[[Attribute](#Attribute)]? | |
| nextToken | str? | |

## ListClustersRequest



| name | type | description |
| :--- | :--- | :--- |
| maxResults | int? | |
| nextToken | str? | |

## ListClustersResponse



| name | type | description |
| :--- | :--- | :--- |
| clusterArns | list[str]? | |
| nextToken | str? | |

## ListContainerInstancesRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| filter | str? | |
| maxResults | int? | |
| nextToken | str? | |
| status |  `ACTIVE, DRAINING, REGISTERING, DEREGISTERING, REGISTRATION_FAILED` ? | |

## ListContainerInstancesResponse



| name | type | description |
| :--- | :--- | :--- |
| containerInstanceArns | list[str]? | |
| nextToken | str? | |

## ListServicesRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| launchType |  `EC2, FARGATE` ? | |
| maxResults | int? | |
| nextToken | str? | |
| schedulingStrategy |  `REPLICA, DAEMON` ? | |

## ListServicesResponse



| name | type | description |
| :--- | :--- | :--- |
| nextToken | str? | |
| serviceArns | list[str]? | |

## ListTagsForResourceRequest



| name | type | description |
| :--- | :--- | :--- |
| resourceArn | str | |

## ListTagsForResourceResponse



| name | type | description |
| :--- | :--- | :--- |
| tags | list[[Tag](#Tag)]? | |

## ListTaskDefinitionFamiliesRequest



| name | type | description |
| :--- | :--- | :--- |
| familyPrefix | str? | |
| maxResults | int? | |
| nextToken | str? | |
| status |  `ACTIVE, INACTIVE, ALL` ? | |

## ListTaskDefinitionFamiliesResponse



| name | type | description |
| :--- | :--- | :--- |
| families | list[str]? | |
| nextToken | str? | |

## ListTaskDefinitionsRequest



| name | type | description |
| :--- | :--- | :--- |
| familyPrefix | str? | |
| maxResults | int? | |
| nextToken | str? | |
| sort |  `ASC, DESC` ? | |
| status |  `ACTIVE, INACTIVE` ? | |

## ListTaskDefinitionsResponse



| name | type | description |
| :--- | :--- | :--- |
| nextToken | str? | |
| taskDefinitionArns | list[str]? | |

## ListTasksRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstance | str? | |
| desiredStatus |  `RUNNING, PENDING, STOPPED` ? | |
| family | str? | |
| launchType |  `EC2, FARGATE` ? | |
| maxResults | int? | |
| nextToken | str? | |
| serviceName | str? | |
| startedBy | str? | |

## ListTasksResponse



| name | type | description |
| :--- | :--- | :--- |
| nextToken | str? | |
| taskArns | list[str]? | |

## PlatformDevice

The devices that are available on the container instance. The only supported device type is a GPU.

| name | type | description |
| :--- | :--- | :--- |
| id | str | |
| type |  `GPU`  | |

## PutAccountSettingDefaultRequest



| name | type | description |
| :--- | :--- | :--- |
| name |  `serviceLongArnFormat, taskLongArnFormat, containerInstanceLongArnFormat, awsvpcTrunking, containerInsights`  | |
| value | str | |

## PutAccountSettingDefaultResponse



| name | type | description |
| :--- | :--- | :--- |
| setting | [Setting](#Setting)? | |

## PutAccountSettingRequest



| name | type | description |
| :--- | :--- | :--- |
| name |  `serviceLongArnFormat, taskLongArnFormat, containerInstanceLongArnFormat, awsvpcTrunking, containerInsights`  | |
| principalArn | str? | |
| value | str | |

## PutAccountSettingResponse



| name | type | description |
| :--- | :--- | :--- |
| setting | [Setting](#Setting)? | |

## PutAttributesRequest



| name | type | description |
| :--- | :--- | :--- |
| attributes | list[[Attribute](#Attribute)] | |
| cluster | str? | |

## PutAttributesResponse



| name | type | description |
| :--- | :--- | :--- |
| attributes | list[[Attribute](#Attribute)]? | |

## PutClusterCapacityProvidersRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviders | list[str] | |
| cluster | str | |
| defaultCapacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)] | |

## PutClusterCapacityProvidersResponse



| name | type | description |
| :--- | :--- | :--- |
| cluster | [Cluster](#Cluster)? | |

## RegisterContainerInstanceRequest



| name | type | description |
| :--- | :--- | :--- |
| attributes | list[[Attribute](#Attribute)]? | |
| cluster | str? | |
| containerInstanceArn | str? | |
| instanceIdentityDocument | str? | |
| instanceIdentityDocumentSignature | str? | |
| platformDevices | list[[PlatformDevice](#PlatformDevice)]? | |
| tags | list[[Tag](#Tag)]? | |
| totalResources | list[[Resource](#Resource)]? | |
| versionInfo | [VersionInfo](#VersionInfo)? | |

## RegisterContainerInstanceResponse



| name | type | description |
| :--- | :--- | :--- |
| containerInstance | [ContainerInstance](#ContainerInstance)? | |

## RegisterTaskDefinitionRequest



| name | type | description |
| :--- | :--- | :--- |
| containerDefinitions | list[[ContainerDefinition](#ContainerDefinition)] | |
| cpu | str? | |
| executionRoleArn | str? | |
| family | str | |
| inferenceAccelerators | list[[InferenceAccelerator](#InferenceAccelerator)]? | |
| ipcMode |  `host, task, none` ? | |
| memory | str? | |
| networkMode |  `bridge, host, awsvpc, none` ? | |
| pidMode |  `host, task` ? | |
| placementConstraints | list[[TaskDefinitionPlacementConstraint](#TaskDefinitionPlacementConstraint)]? | |
| proxyConfiguration | [ProxyConfiguration](#ProxyConfiguration)? | |
| requiresCompatibilities | list[ `EC2, FARGATE` ]? | |
| tags | list[[Tag](#Tag)]? | |
| taskRoleArn | str? | |
| volumes | list[[Volume](#Volume)]? | |

## RegisterTaskDefinitionResponse



| name | type | description |
| :--- | :--- | :--- |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | [TaskDefinition](#TaskDefinition)? | |

## RunTaskRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| cluster | str? | |
| count | int? | |
| enableECSManagedTags | bool? | |
| group | str? | |
| launchType |  `EC2, FARGATE` ? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| overrides | [TaskOverride](#TaskOverride)? | |
| placementConstraints | list[[PlacementConstraint](#PlacementConstraint)]? | |
| placementStrategy | list[[PlacementStrategy](#PlacementStrategy)]? | |
| platformVersion | str? | |
| propagateTags |  `TASK_DEFINITION, SERVICE` ? | |
| referenceId | str? | |
| startedBy | str? | |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | str | |

## RunTaskResponse



| name | type | description |
| :--- | :--- | :--- |
| failures | list[[Failure](#Failure)]? | |
| tasks | list[[Task](#Task)]? | |

## StartTaskRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstances | list[str] | |
| enableECSManagedTags | bool? | |
| group | str? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| overrides | [TaskOverride](#TaskOverride)? | |
| propagateTags |  `TASK_DEFINITION, SERVICE` ? | |
| referenceId | str? | |
| startedBy | str? | |
| tags | list[[Tag](#Tag)]? | |
| taskDefinition | str | |

## StartTaskResponse



| name | type | description |
| :--- | :--- | :--- |
| failures | list[[Failure](#Failure)]? | |
| tasks | list[[Task](#Task)]? | |

## StopTaskRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| reason | str? | |
| task | str | |

## StopTaskResponse



| name | type | description |
| :--- | :--- | :--- |
| task | [Task](#Task)? | |

## SubmitAttachmentStateChangesRequest



| name | type | description |
| :--- | :--- | :--- |
| attachments | list[[AttachmentStateChange](#AttachmentStateChange)] | |
| cluster | str? | |

## SubmitAttachmentStateChangesResponse



| name | type | description |
| :--- | :--- | :--- |
| acknowledgment | str? | |

## SubmitContainerStateChangeRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerName | str? | |
| exitCode | int? | |
| networkBindings | list[[NetworkBinding](#NetworkBinding)]? | |
| reason | str? | |
| runtimeId | str? | |
| status | str? | |
| task | str? | |

## SubmitContainerStateChangeResponse



| name | type | description |
| :--- | :--- | :--- |
| acknowledgment | str? | |

## SubmitTaskStateChangeRequest



| name | type | description |
| :--- | :--- | :--- |
| attachments | list[[AttachmentStateChange](#AttachmentStateChange)]? | |
| cluster | str? | |
| containers | list[[ContainerStateChange](#ContainerStateChange)]? | |
| executionStoppedAt | [datetime](#datetime)? | |
| pullStartedAt | [datetime](#datetime)? | |
| pullStoppedAt | [datetime](#datetime)? | |
| reason | str? | |
| status | str? | |
| task | str? | |

## SubmitTaskStateChangeResponse



| name | type | description |
| :--- | :--- | :--- |
| acknowledgment | str? | |

## TagResourceRequest



| name | type | description |
| :--- | :--- | :--- |
| resourceArn | str | |
| tags | list[[Tag](#Tag)] | |

## UntagResourceRequest



| name | type | description |
| :--- | :--- | :--- |
| resourceArn | str | |
| tagKeys | list[str] | |

## UpdateCapacityProviderRequest



| name | type | description |
| :--- | :--- | :--- |
| autoScalingGroupProvider | [AutoScalingGroupProviderUpdate](#AutoScalingGroupProviderUpdate) | |
| name | str | |

## UpdateCapacityProviderResponse



| name | type | description |
| :--- | :--- | :--- |
| capacityProvider | [CapacityProvider](#CapacityProvider)? | |

## UpdateClusterSettingsRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str | |
| settings | list[[ClusterSetting](#ClusterSetting)] | |

## UpdateClusterSettingsResponse



| name | type | description |
| :--- | :--- | :--- |
| cluster | [Cluster](#Cluster)? | |

## UpdateContainerAgentRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstance | str | |

## UpdateContainerAgentResponse



| name | type | description |
| :--- | :--- | :--- |
| containerInstance | [ContainerInstance](#ContainerInstance)? | |

## UpdateContainerInstancesStateRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str? | |
| containerInstances | list[str] | |
| status |  `ACTIVE, DRAINING, REGISTERING, DEREGISTERING, REGISTRATION_FAILED`  | |

## UpdateContainerInstancesStateResponse



| name | type | description |
| :--- | :--- | :--- |
| containerInstances | list[[ContainerInstance](#ContainerInstance)]? | |
| failures | list[[Failure](#Failure)]? | |

## UpdateServicePrimaryTaskSetRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str | |
| primaryTaskSet | str | |
| service | str | |

## UpdateServicePrimaryTaskSetResponse



| name | type | description |
| :--- | :--- | :--- |
| taskSet | [TaskSet](#TaskSet)? | |

## UpdateServiceRequest



| name | type | description |
| :--- | :--- | :--- |
| capacityProviderStrategy | list[[CapacityProviderStrategyItem](#CapacityProviderStrategyItem)]? | |
| cluster | str? | |
| deploymentConfiguration | [DeploymentConfiguration](#DeploymentConfiguration)? | |
| desiredCount | int? | |
| forceNewDeployment | bool? | |
| healthCheckGracePeriodSeconds | int? | |
| networkConfiguration | [NetworkConfiguration](#NetworkConfiguration)? | |
| placementConstraints | list[[PlacementConstraint](#PlacementConstraint)]? | |
| placementStrategy | list[[PlacementStrategy](#PlacementStrategy)]? | |
| platformVersion | str? | |
| service | str | |
| taskDefinition | str? | |

## UpdateServiceResponse



| name | type | description |
| :--- | :--- | :--- |
| service | [Service](#Service)? | |

## UpdateTaskSetRequest



| name | type | description |
| :--- | :--- | :--- |
| cluster | str | |
| scale | [Scale](#Scale) | |
| service | str | |
| taskSet | str | |

## UpdateTaskSetResponse



| name | type | description |
| :--- | :--- | :--- |
| taskSet | [TaskSet](#TaskSet)? | |
