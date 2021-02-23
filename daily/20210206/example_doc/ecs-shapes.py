from __future__ import annotations
import typing
from metashape.declarative import field
import datetime


class Attachment:
    """
    An object representing a container instance or task attachment.
    """

    details: typing.Optional[typing.List[KeyValuePair]]
    id: typing.Optional[str]
    status: typing.Optional[str]
    type: typing.Optional[str]


class KeyValuePair:
    """
    A key-value pair object.
    """

    name: typing.Optional[str]
    value: typing.Optional[str]


class AttachmentStateChange:
    """
    An object representing a change in state for a task attachment.
    """

    attachmentArn: str
    status: str


class Attribute:
    """
    An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    name: str
    targetId: typing.Optional[str]
    targetType: typing.Optional[typing.Literal['container-instance']] = field()
    value: typing.Optional[str]


class AutoScalingGroupProvider:
    """
    The details of the Auto Scaling group for the capacity provider.
    """

    autoScalingGroupArn: str
    managedScaling: typing.Optional[ManagedScaling] = field(metadata={'openapi': {'description': '<p>The managed scaling settings for the Auto Scaling group capacity provider.</p> <p>When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified <code>targetCapacity</code> value as the target value for the metric. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling">Using Managed Scaling</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.</p>'}})
    managedTerminationProtection: typing.Optional[typing.Literal['ENABLED', 'DISABLED']] = field()


class ManagedScaling:
    """
    <p>The managed scaling settings for the Auto Scaling group capacity provider.</p> <p>When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified <code>targetCapacity</code> value as the target value for the metric. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling">Using Managed Scaling</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.</p>
    """

    instanceWarmupPeriod: typing.Optional[int] = field(metadata={'openapi': {'maximum': 10000, 'minimum': 0}})
    maximumScalingStepSize: typing.Optional[int] = field(metadata={'openapi': {'maximum': 10000, 'minimum': 1}})
    minimumScalingStepSize: typing.Optional[int] = field(metadata={'openapi': {'maximum': 10000, 'minimum': 1}})
    status: typing.Optional[typing.Literal['ENABLED', 'DISABLED']] = field()
    targetCapacity: typing.Optional[int] = field(metadata={'openapi': {'maximum': 100, 'minimum': 1}})


class AutoScalingGroupProviderUpdate:
    """
    The details of the Auto Scaling group capacity provider to update.
    """

    managedScaling: typing.Optional[ManagedScaling] = field(metadata={'openapi': {'description': '<p>The managed scaling settings for the Auto Scaling group capacity provider.</p> <p>When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified <code>targetCapacity</code> value as the target value for the metric. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling">Using Managed Scaling</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.</p>'}})
    managedTerminationProtection: typing.Optional[typing.Literal['ENABLED', 'DISABLED']] = field()


class AwsVpcConfiguration:
    """
    An object representing the networking details for a task or service.
    """

    assignPublicIp: typing.Optional[typing.Literal['ENABLED', 'DISABLED']] = field()
    securityGroups: typing.Optional[typing.List[str]]
    subnets: typing.List[str]


class CapacityProvider:
    """
    The details of a capacity provider.
    """

    autoScalingGroupProvider: typing.Optional[AutoScalingGroupProvider] = field(metadata={'openapi': {'description': 'The details of the Auto Scaling group for the capacity provider.'}})
    capacityProviderArn: typing.Optional[str]
    name: typing.Optional[str]
    status: typing.Optional[typing.Literal['ACTIVE', 'INACTIVE']] = field()
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    updateStatus: typing.Optional[typing.Literal['DELETE_IN_PROGRESS', 'DELETE_COMPLETE', 'DELETE_FAILED', 'UPDATE_IN_PROGRESS', 'UPDATE_COMPLETE', 'UPDATE_FAILED']] = field()
    updateStatusReason: typing.Optional[str]


class Tag:
    """
    <p>The metadata that you apply to a resource to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.</p> <p>The following basic restrictions apply to tags:</p> <ul> <li> <p>Maximum number of tags per resource - 50</p> </li> <li> <p>For each resource, each tag key must be unique, and each tag key can have only one value.</p> </li> <li> <p>Maximum key length - 128 Unicode characters in UTF-8</p> </li> <li> <p>Maximum value length - 256 Unicode characters in UTF-8</p> </li> <li> <p>If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.</p> </li> <li> <p>Tag keys and values are case-sensitive.</p> </li> <li> <p>Do not use <code>aws:</code>, <code>AWS:</code>, or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.</p> </li> </ul>
    """

    key: typing.Optional[str] = field(metadata={'openapi': {'maxLength': 128, 'minLength': 1, 'pattern': '^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-@]*)$'}})
    value: typing.Optional[str] = field(metadata={'openapi': {'maxLength': 256, 'minLength': 0, 'pattern': '^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-@]*)$'}})


class CapacityProviderStrategyItem:
    """
    The details of a capacity provider strategy.
    """

    base: typing.Optional[int] = field(metadata={'openapi': {'maximum': 100000, 'minimum': 0}})
    capacityProvider: str
    weight: typing.Optional[int] = field(metadata={'openapi': {'maximum': 1000, 'minimum': 0}})


class Cluster:
    """
    A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.
    """

    activeServicesCount: typing.Optional[int]
    attachments: typing.Optional[typing.List[Attachment]]
    attachmentsStatus: typing.Optional[str]
    capacityProviders: typing.Optional[typing.List[str]]
    clusterArn: typing.Optional[str]
    clusterName: typing.Optional[str]
    defaultCapacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    pendingTasksCount: typing.Optional[int]
    registeredContainerInstancesCount: typing.Optional[int]
    runningTasksCount: typing.Optional[int]
    settings: typing.Optional[typing.List[ClusterSetting]]
    statistics: typing.Optional[typing.List[KeyValuePair]]
    status: typing.Optional[str]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})


class ClusterSetting:
    """
    The settings to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster.
    """

    name: typing.Optional[typing.Literal['containerInsights']] = field()
    value: typing.Optional[str]


class Container:
    """
    A Docker container that is part of a task.
    """

    containerArn: typing.Optional[str]
    cpu: typing.Optional[str]
    exitCode: typing.Optional[int]
    gpuIds: typing.Optional[typing.List[str]]
    healthStatus: typing.Optional[typing.Literal['HEALTHY', 'UNHEALTHY', 'UNKNOWN']] = field()
    image: typing.Optional[str]
    imageDigest: typing.Optional[str]
    lastStatus: typing.Optional[str]
    memory: typing.Optional[str]
    memoryReservation: typing.Optional[str]
    name: typing.Optional[str]
    networkBindings: typing.Optional[typing.List[NetworkBinding]]
    networkInterfaces: typing.Optional[typing.List[NetworkInterface]]
    reason: typing.Optional[str]
    runtimeId: typing.Optional[str]
    taskArn: typing.Optional[str]


class NetworkInterface:
    """
    An object representing the elastic network interface for tasks that use the <code>awsvpc</code> network mode.
    """

    attachmentId: typing.Optional[str]
    ipv6Address: typing.Optional[str]
    privateIpv4Address: typing.Optional[str]


class NetworkBinding:
    """
    Details on the network bindings between a container and its host container instance. After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.
    """

    bindIP: typing.Optional[str]
    containerPort: typing.Optional[int]
    hostPort: typing.Optional[int]
    protocol: typing.Optional[typing.Literal['tcp', 'udp']] = field()


class ContainerDefinition:
    """
    Container definitions are used in task definitions to describe the different containers that are launched as part of a task.
    """

    command: typing.Optional[typing.List[str]]
    cpu: typing.Optional[int]
    dependsOn: typing.Optional[typing.List[ContainerDependency]]
    disableNetworking: typing.Optional[bool]
    dnsSearchDomains: typing.Optional[typing.List[str]]
    dnsServers: typing.Optional[typing.List[str]]
    dockerLabels: typing.Optional[typing.Dict[str, str]]
    dockerSecurityOptions: typing.Optional[typing.List[str]]
    entryPoint: typing.Optional[typing.List[str]]
    environment: typing.Optional[typing.List[KeyValuePair]]
    environmentFiles: typing.Optional[typing.List[EnvironmentFile]]
    essential: typing.Optional[bool]
    extraHosts: typing.Optional[typing.List[HostEntry]]
    firelensConfiguration: typing.Optional[FirelensConfiguration] = field(metadata={'openapi': {'description': 'The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html">Custom Log Routing</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.'}})
    healthCheck: typing.Optional[HealthCheck] = field(metadata={'openapi': {'description': '<p>An object representing a container health check. Health check parameters that are specified in a container definition override any Docker health checks that exist in the container image (such as those specified in a parent image or from the image\'s Dockerfile).</p> <p>You can view the health status of both individual containers and a task with the DescribeTasks API operation or when viewing the task details in the console.</p> <p>The following describes the possible <code>healthStatus</code> values for a container:</p> <ul> <li> <p> <code>HEALTHY</code>-The container health check has passed successfully.</p> </li> <li> <p> <code>UNHEALTHY</code>-The container health check has failed.</p> </li> <li> <p> <code>UNKNOWN</code>-The container health check is being evaluated or there is no container health check defined.</p> </li> </ul> <p>The following describes the possible <code>healthStatus</code> values for a task. The container health check status of nonessential containers do not have an effect on the health status of a task.</p> <ul> <li> <p> <code>HEALTHY</code>-All essential containers within the task have passed their health checks.</p> </li> <li> <p> <code>UNHEALTHY</code>-One or more essential containers have failed their health check.</p> </li> <li> <p> <code>UNKNOWN</code>-The essential containers within the task are still having their health checks evaluated or there are no container health checks defined.</p> </li> </ul> <p>If a task is run manually, and not as part of a service, the task will continue its lifecycle regardless of its health status. For tasks that are part of a service, if the task reports as unhealthy then the task will be stopped and the service scheduler will replace it.</p> <p>The following are notes about container health check support:</p> <ul> <li> <p>Container health checks require version 1.17.0 or greater of the Amazon ECS container agent. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html">Updating the Amazon ECS Container Agent</a>.</p> </li> <li> <p>Container health checks are supported for Fargate tasks if you are using platform version 1.1.0 or greater. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html">AWS Fargate Platform Versions</a>.</p> </li> <li> <p>Container health checks are not supported for tasks that are part of a service that is configured to use a Classic Load Balancer.</p> </li> </ul>'}})
    hostname: typing.Optional[str]
    image: typing.Optional[str]
    interactive: typing.Optional[bool]
    links: typing.Optional[typing.List[str]]
    linuxParameters: typing.Optional[LinuxParameters] = field(metadata={'openapi': {'description': 'Linux-specific options that are applied to the container, such as Linux <a>KernelCapabilities</a>.'}})
    logConfiguration: typing.Optional[LogConfiguration] = field(metadata={'openapi': {'description': '<p>The log configuration for the container. This parameter maps to <code>LogConfig</code> in the <a href="https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate">Create a container</a> section of the <a href="https://docs.docker.com/engine/api/v1.35/">Docker Remote API</a> and the <code>--log-driver</code> option to <a href="https://docs.docker.com/engine/reference/commandline/run/"> <code>docker run</code> </a>.</p> <p>By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver configuration in the container definition. For more information on the options for different supported log drivers, see <a href="https://docs.docker.com/engine/admin/logging/overview/">Configure logging drivers</a> in the Docker documentation.</p> <p>The following should be noted when specifying a log configuration for your containers:</p> <ul> <li> <p>Amazon ECS currently supports a subset of the logging drivers available to the Docker daemon (shown in the valid values below). Additional log drivers may be available in future releases of the Amazon ECS container agent.</p> </li> <li> <p>This parameter requires version 1.18 of the Docker Remote API or greater on your container instance.</p> </li> <li> <p>For tasks hosted on Amazon EC2 instances, the Amazon ECS container agent must register the available logging drivers with the <code>ECS_AVAILABLE_LOGGING_DRIVERS</code> environment variable before containers placed on that instance can use these log configuration options. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html">Amazon ECS container agent configuration</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </li> <li> <p>For tasks on AWS Fargate, because you do not have access to the underlying infrastructure your tasks are hosted on, any additional software needed will have to be installed outside of the task. For example, the Fluentd output aggregators or a remote host running Logstash to send Gelf logs to.</p> </li> </ul>'}})
    memory: typing.Optional[int]
    memoryReservation: typing.Optional[int]
    mountPoints: typing.Optional[typing.List[MountPoint]]
    name: typing.Optional[str]
    portMappings: typing.Optional[typing.List[PortMapping]]
    privileged: typing.Optional[bool]
    pseudoTerminal: typing.Optional[bool]
    readonlyRootFilesystem: typing.Optional[bool]
    repositoryCredentials: typing.Optional[RepositoryCredentials] = field(metadata={'openapi': {'description': 'The repository credentials for private registry authentication.'}})
    resourceRequirements: typing.Optional[typing.List[ResourceRequirement]]
    secrets: typing.Optional[typing.List[Secret]]
    startTimeout: typing.Optional[int]
    stopTimeout: typing.Optional[int]
    systemControls: typing.Optional[typing.List[SystemControl]]
    ulimits: typing.Optional[typing.List[Ulimit]]
    user: typing.Optional[str]
    volumesFrom: typing.Optional[typing.List[VolumeFrom]]
    workingDirectory: typing.Optional[str]


class VolumeFrom:
    """
    Details on a data volume from another container in the same task definition.
    """

    readOnly: typing.Optional[bool]
    sourceContainer: typing.Optional[str]


class Ulimit:
    """
    The <code>ulimit</code> settings to pass to the container.
    """

    hardLimit: int
    name: typing.Literal['core', 'cpu', 'data', 'fsize', 'locks', 'memlock', 'msgqueue', 'nice', 'nofile', 'nproc', 'rss', 'rtprio', 'rttime', 'sigpending', 'stack'] = field()
    softLimit: int


class SystemControl:
    """
    <p>A list of namespaced kernel parameters to set in the container. This parameter maps to <code>Sysctls</code> in the <a href="https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate">Create a container</a> section of the <a href="https://docs.docker.com/engine/api/v1.35/">Docker Remote API</a> and the <code>--sysctl</code> option to <a href="https://docs.docker.com/engine/reference/run/#security-configuration">docker run</a>.</p> <p>It is not recommended that you specify network-related <code>systemControls</code> parameters for multiple containers in a single task that also uses either the <code>awsvpc</code> or <code>host</code> network mode for the following reasons:</p> <ul> <li> <p>For tasks that use the <code>awsvpc</code> network mode, if you set <code>systemControls</code> for any container, it applies to all containers in the task. If you set different <code>systemControls</code> for multiple containers in a single task, the container that is started last determines which <code>systemControls</code> take effect.</p> </li> <li> <p>For tasks that use the <code>host</code> network mode, the <code>systemControls</code> parameter applies to the container instance's kernel parameter as well as that of all containers of any tasks running on that container instance.</p> </li> </ul>
    """

    namespace: typing.Optional[str]
    value: typing.Optional[str]


class Secret:
    """
    <p>An object representing the secret to expose to your container. Secrets can be exposed to a container in the following ways:</p> <ul> <li> <p>To inject sensitive data into your containers as environment variables, use the <code>secrets</code> container definition parameter.</p> </li> <li> <p>To reference sensitive information in the log configuration of a container, use the <code>secretOptions</code> container definition parameter.</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html">Specifying Sensitive Data</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
    """

    name: str
    valueFrom: str


class ResourceRequirement:
    """
    The type and amount of a resource to assign to a container. The supported resource types are GPUs and Elastic Inference accelerators. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html">Working with GPUs on Amazon ECS</a> or <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html">Working with Amazon Elastic Inference on Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i> 
    """

    type: typing.Literal['GPU', 'InferenceAccelerator'] = field()
    value: str


class RepositoryCredentials:
    """
    The repository credentials for private registry authentication.
    """

    credentialsParameter: str


class PortMapping:
    """
    <p>Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.</p> <p>If you are using containers in a task with the <code>awsvpc</code> or <code>host</code> network mode, exposed ports should be specified using <code>containerPort</code>. The <code>hostPort</code> can be left blank or it must be the same value as the <code>containerPort</code>.</p> <p>After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>
    """

    containerPort: typing.Optional[int]
    hostPort: typing.Optional[int]
    protocol: typing.Optional[typing.Literal['tcp', 'udp']] = field()


class MountPoint:
    """
    Details on a volume mount point that is used in a container definition.
    """

    containerPath: typing.Optional[str]
    readOnly: typing.Optional[bool]
    sourceVolume: typing.Optional[str]


class LogConfiguration:
    """
    <p>The log configuration for the container. This parameter maps to <code>LogConfig</code> in the <a href="https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate">Create a container</a> section of the <a href="https://docs.docker.com/engine/api/v1.35/">Docker Remote API</a> and the <code>--log-driver</code> option to <a href="https://docs.docker.com/engine/reference/commandline/run/"> <code>docker run</code> </a>.</p> <p>By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver configuration in the container definition. For more information on the options for different supported log drivers, see <a href="https://docs.docker.com/engine/admin/logging/overview/">Configure logging drivers</a> in the Docker documentation.</p> <p>The following should be noted when specifying a log configuration for your containers:</p> <ul> <li> <p>Amazon ECS currently supports a subset of the logging drivers available to the Docker daemon (shown in the valid values below). Additional log drivers may be available in future releases of the Amazon ECS container agent.</p> </li> <li> <p>This parameter requires version 1.18 of the Docker Remote API or greater on your container instance.</p> </li> <li> <p>For tasks hosted on Amazon EC2 instances, the Amazon ECS container agent must register the available logging drivers with the <code>ECS_AVAILABLE_LOGGING_DRIVERS</code> environment variable before containers placed on that instance can use these log configuration options. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html">Amazon ECS container agent configuration</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </li> <li> <p>For tasks on AWS Fargate, because you do not have access to the underlying infrastructure your tasks are hosted on, any additional software needed will have to be installed outside of the task. For example, the Fluentd output aggregators or a remote host running Logstash to send Gelf logs to.</p> </li> </ul>
    """

    logDriver: typing.Literal['json-file', 'syslog', 'journald', 'gelf', 'fluentd', 'awslogs', 'splunk', 'awsfirelens'] = field()
    options: typing.Optional[typing.Dict[str, str]]
    secretOptions: typing.Optional[typing.List[Secret]]


class LinuxParameters:
    """
    Linux-specific options that are applied to the container, such as Linux <a>KernelCapabilities</a>.
    """

    capabilities: typing.Optional[KernelCapabilities] = field(metadata={'openapi': {'description': 'The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <a href="https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities">Runtime privilege and Linux capabilities</a> in the <i>Docker run reference</i>. For more detailed information on these Linux capabilities, see the <a href="http://man7.org/linux/man-pages/man7/capabilities.7.html">capabilities(7)</a> Linux manual page.'}})
    devices: typing.Optional[typing.List[Device]]
    initProcessEnabled: typing.Optional[bool]
    maxSwap: typing.Optional[int]
    sharedMemorySize: typing.Optional[int]
    swappiness: typing.Optional[int]
    tmpfs: typing.Optional[typing.List[Tmpfs]]


class Tmpfs:
    """
    The container path, mount options, and size of the tmpfs mount.
    """

    containerPath: str
    mountOptions: typing.Optional[typing.List[str]]
    size: int


class Device:
    """
    An object representing a container instance host device.
    """

    containerPath: typing.Optional[str]
    hostPath: str
    permissions: typing.Optional[typing.List[typing.Literal['read', 'write', 'mknod']]]


class KernelCapabilities:
    """
    The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <a href="https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities">Runtime privilege and Linux capabilities</a> in the <i>Docker run reference</i>. For more detailed information on these Linux capabilities, see the <a href="http://man7.org/linux/man-pages/man7/capabilities.7.html">capabilities(7)</a> Linux manual page.
    """

    add: typing.Optional[typing.List[str]]
    drop: typing.Optional[typing.List[str]]


class HealthCheck:
    """
    <p>An object representing a container health check. Health check parameters that are specified in a container definition override any Docker health checks that exist in the container image (such as those specified in a parent image or from the image's Dockerfile).</p> <p>You can view the health status of both individual containers and a task with the DescribeTasks API operation or when viewing the task details in the console.</p> <p>The following describes the possible <code>healthStatus</code> values for a container:</p> <ul> <li> <p> <code>HEALTHY</code>-The container health check has passed successfully.</p> </li> <li> <p> <code>UNHEALTHY</code>-The container health check has failed.</p> </li> <li> <p> <code>UNKNOWN</code>-The container health check is being evaluated or there is no container health check defined.</p> </li> </ul> <p>The following describes the possible <code>healthStatus</code> values for a task. The container health check status of nonessential containers do not have an effect on the health status of a task.</p> <ul> <li> <p> <code>HEALTHY</code>-All essential containers within the task have passed their health checks.</p> </li> <li> <p> <code>UNHEALTHY</code>-One or more essential containers have failed their health check.</p> </li> <li> <p> <code>UNKNOWN</code>-The essential containers within the task are still having their health checks evaluated or there are no container health checks defined.</p> </li> </ul> <p>If a task is run manually, and not as part of a service, the task will continue its lifecycle regardless of its health status. For tasks that are part of a service, if the task reports as unhealthy then the task will be stopped and the service scheduler will replace it.</p> <p>The following are notes about container health check support:</p> <ul> <li> <p>Container health checks require version 1.17.0 or greater of the Amazon ECS container agent. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html">Updating the Amazon ECS Container Agent</a>.</p> </li> <li> <p>Container health checks are supported for Fargate tasks if you are using platform version 1.1.0 or greater. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html">AWS Fargate Platform Versions</a>.</p> </li> <li> <p>Container health checks are not supported for tasks that are part of a service that is configured to use a Classic Load Balancer.</p> </li> </ul>
    """

    command: typing.List[str]
    interval: typing.Optional[int]
    retries: typing.Optional[int]
    startPeriod: typing.Optional[int]
    timeout: typing.Optional[int]


class FirelensConfiguration:
    """
    The FireLens configuration for the container. This is used to specify and configure a log router for container logs. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html">Custom Log Routing</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    options: typing.Optional[typing.Dict[str, str]]
    type: typing.Literal['fluentd', 'fluentbit'] = field()


class HostEntry:
    """
    Hostnames and IP address entries that are added to the <code>/etc/hosts</code> file of a container via the <code>extraHosts</code> parameter of its <a>ContainerDefinition</a>. 
    """

    hostname: str
    ipAddress: str


class EnvironmentFile:
    """
    <p>A list of files containing the environment variables to pass to a container. You can specify up to ten environment files. The file must have a <code>.env</code> file extension. Each line in an environment file should contain an environment variable in <code>VARIABLE=VALUE</code> format. Lines beginning with <code>#</code> are treated as comments and are ignored. For more information on the environment variable file syntax, see <a href="https://docs.docker.com/compose/env-file/">Declare default environment variables in file</a>.</p> <p>If there are environment variables specified using the <code>environment</code> parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html">Specifying Environment Variables</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>This field is not valid for containers in tasks using the Fargate launch type.</p>
    """

    type: typing.Literal['s3'] = field()
    value: str


class ContainerDependency:
    """
    <p>The dependencies defined for container startup and shutdown. A container can contain multiple dependencies. When a dependency is defined for container startup, for container shutdown it is reversed.</p> <p>Your Amazon ECS container instances require at least version 1.26.0 of the container agent to enable container dependencies. However, we recommend using the latest container agent version. For information about checking your agent version and updating to the latest version, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html">Updating the Amazon ECS Container Agent</a> in the <i>Amazon Elastic Container Service Developer Guide</i>. If you are using an Amazon ECS-optimized Linux AMI, your instance needs at least version 1.26.0-1 of the <code>ecs-init</code> package. If your container instances are launched from version <code>20190301</code> or later, then they contain the required versions of the container agent and <code>ecs-init</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html">Amazon ECS-optimized Linux AMI</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <note> <p>For tasks using the Fargate launch type, this parameter requires that the task or service uses platform version 1.3.0 or later.</p> </note>
    """

    condition: typing.Literal['START', 'COMPLETE', 'SUCCESS', 'HEALTHY'] = field()
    containerName: str


class ContainerInstance:
    """
    An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
    """

    agentConnected: typing.Optional[bool]
    agentUpdateStatus: typing.Optional[typing.Literal['PENDING', 'STAGING', 'STAGED', 'UPDATING', 'UPDATED', 'FAILED']] = field()
    attachments: typing.Optional[typing.List[Attachment]]
    attributes: typing.Optional[typing.List[Attribute]]
    capacityProviderName: typing.Optional[str]
    containerInstanceArn: typing.Optional[str]
    ec2InstanceId: typing.Optional[str]
    pendingTasksCount: typing.Optional[int]
    registeredAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    registeredResources: typing.Optional[typing.List[Resource]]
    remainingResources: typing.Optional[typing.List[Resource]]
    runningTasksCount: typing.Optional[int]
    status: typing.Optional[str]
    statusReason: typing.Optional[str]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    version: typing.Optional[int]
    versionInfo: typing.Optional[VersionInfo] = field(metadata={'openapi': {'description': 'The Docker and Amazon ECS container agent version information about a container instance.'}})


class VersionInfo:
    """
    The Docker and Amazon ECS container agent version information about a container instance.
    """

    agentHash: typing.Optional[str]
    agentVersion: typing.Optional[str]
    dockerVersion: typing.Optional[str]


class Resource:
    """
    Describes the resources available for a container instance.
    """

    doubleValue: typing.Optional[float] = field(metadata={'openapi': {'format': 'double'}})
    integerValue: typing.Optional[int]
    longValue: typing.Optional[int]
    name: typing.Optional[str]
    stringSetValue: typing.Optional[typing.List[str]]
    type: typing.Optional[str]


class ContainerOverride:
    """
    The overrides that should be sent to a container. An empty container override can be passed in. An example of an empty container override would be <code>{"containerOverrides": [ ] }</code>. If a non-empty container override is specified, the <code>name</code> parameter must be included.
    """

    command: typing.Optional[typing.List[str]]
    cpu: typing.Optional[int]
    environment: typing.Optional[typing.List[KeyValuePair]]
    environmentFiles: typing.Optional[typing.List[EnvironmentFile]]
    memory: typing.Optional[int]
    memoryReservation: typing.Optional[int]
    name: typing.Optional[str]
    resourceRequirements: typing.Optional[typing.List[ResourceRequirement]]


class ContainerStateChange:
    """
    An object representing a change in state for a container.
    """

    containerName: typing.Optional[str]
    exitCode: typing.Optional[int]
    imageDigest: typing.Optional[str]
    networkBindings: typing.Optional[typing.List[NetworkBinding]]
    reason: typing.Optional[str]
    runtimeId: typing.Optional[str]
    status: typing.Optional[str]


class CreateCapacityProviderRequest:
    autoScalingGroupProvider: AutoScalingGroupProvider = field(metadata={'openapi': {'description': 'The details of the Auto Scaling group for the capacity provider.'}})
    name: str
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})


class CreateCapacityProviderResponse:
    capacityProvider: typing.Optional[CapacityProvider] = field(metadata={'openapi': {'description': 'The details of a capacity provider.'}})


class CreateClusterRequest:
    capacityProviders: typing.Optional[typing.List[str]]
    clusterName: typing.Optional[str]
    defaultCapacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    settings: typing.Optional[typing.List[ClusterSetting]]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})


class CreateClusterResponse:
    cluster: typing.Optional[Cluster] = field(metadata={'openapi': {'description': 'A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.'}})


class CreateServiceRequest:
    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    clientToken: typing.Optional[str]
    cluster: typing.Optional[str]
    deploymentConfiguration: typing.Optional[DeploymentConfiguration] = field(metadata={'openapi': {'description': 'Optional deployment parameters that control how many tasks run during a deployment and the ordering of stopping and starting tasks.'}})
    deploymentController: typing.Optional[DeploymentController] = field(metadata={'openapi': {'description': 'The deployment controller to use for the service. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html">Amazon ECS Deployment Types</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.'}})
    desiredCount: typing.Optional[int]
    enableECSManagedTags: typing.Optional[bool]
    healthCheckGracePeriodSeconds: typing.Optional[int]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    loadBalancers: typing.Optional[typing.List[LoadBalancer]]
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    placementConstraints: typing.Optional[typing.List[PlacementConstraint]]
    placementStrategy: typing.Optional[typing.List[PlacementStrategy]]
    platformVersion: typing.Optional[str]
    propagateTags: typing.Optional[typing.Literal['TASK_DEFINITION', 'SERVICE']] = field()
    role: typing.Optional[str]
    schedulingStrategy: typing.Optional[typing.Literal['REPLICA', 'DAEMON']] = field()
    serviceName: str
    serviceRegistries: typing.Optional[typing.List[ServiceRegistry]]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: typing.Optional[str]


class ServiceRegistry:
    """
    Details of the service registry.
    """

    containerName: typing.Optional[str]
    containerPort: typing.Optional[int]
    port: typing.Optional[int]
    registryArn: typing.Optional[str]


class PlacementStrategy:
    """
    The task placement strategy for a task or service. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html">Task Placement Strategies</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    field: typing.Optional[str]
    type: typing.Optional[typing.Literal['random', 'spread', 'binpack']] = field()


class PlacementConstraint:
    """
    <p>An object representing a constraint on task placement. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <note> <p>If you are using the Fargate launch type, task placement constraints are not supported.</p> </note>
    """

    expression: typing.Optional[str]
    type: typing.Optional[typing.Literal['distinctInstance', 'memberOf']] = field()


class NetworkConfiguration:
    """
    An object representing the network configuration for a task or service.
    """

    awsvpcConfiguration: typing.Optional[AwsVpcConfiguration] = field(metadata={'openapi': {'description': 'An object representing the networking details for a task or service.'}})


class LoadBalancer:
    """
    <p>The load balancer configuration to use with a service or task set.</p> <p>For specific notes and restrictions regarding the use of load balancers with services and task sets, see the CreateService and CreateTaskSet actions.</p>
    """

    containerName: typing.Optional[str]
    containerPort: typing.Optional[int]
    loadBalancerName: typing.Optional[str]
    targetGroupArn: typing.Optional[str]


class DeploymentController:
    """
    The deployment controller to use for the service. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html">Amazon ECS Deployment Types</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    type: typing.Literal['ECS', 'CODE_DEPLOY', 'EXTERNAL'] = field()


class DeploymentConfiguration:
    """
    Optional deployment parameters that control how many tasks run during a deployment and the ordering of stopping and starting tasks.
    """

    deploymentCircuitBreaker: typing.Optional[DeploymentCircuitBreaker] = field(metadata={'openapi': {'description': '<note> <p>The deployment circuit breaker can only be used for services using the rolling update (<code>ECS</code>) deployment type that are not behind a Classic Load Balancer.</p> </note> <p>The <b>deployment circuit breaker</b> determines whether a service deployment will fail if the service can\'t reach a steady state. If enabled, a service deployment will transition to a failed state and stop launching new tasks. You can also enable Amazon ECS to roll back your service to the last completed deployment after a failure. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html">Rolling update</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>'}})
    maximumPercent: typing.Optional[int]
    minimumHealthyPercent: typing.Optional[int]


class DeploymentCircuitBreaker:
    """
    <note> <p>The deployment circuit breaker can only be used for services using the rolling update (<code>ECS</code>) deployment type that are not behind a Classic Load Balancer.</p> </note> <p>The <b>deployment circuit breaker</b> determines whether a service deployment will fail if the service can't reach a steady state. If enabled, a service deployment will transition to a failed state and stop launching new tasks. You can also enable Amazon ECS to roll back your service to the last completed deployment after a failure. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html">Rolling update</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
    """

    enable: bool
    rollback: bool


class CreateServiceResponse:
    service: typing.Optional[Service] = field(metadata={'openapi': {'description': 'Details on a service within a cluster'}})


class Service:
    """
    Details on a service within a cluster
    """

    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    clusterArn: typing.Optional[str]
    createdAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    createdBy: typing.Optional[str]
    deploymentConfiguration: typing.Optional[DeploymentConfiguration] = field(metadata={'openapi': {'description': 'Optional deployment parameters that control how many tasks run during a deployment and the ordering of stopping and starting tasks.'}})
    deploymentController: typing.Optional[DeploymentController] = field(metadata={'openapi': {'description': 'The deployment controller to use for the service. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html">Amazon ECS Deployment Types</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.'}})
    deployments: typing.Optional[typing.List[Deployment]]
    desiredCount: typing.Optional[int]
    enableECSManagedTags: typing.Optional[bool]
    events: typing.Optional[typing.List[ServiceEvent]]
    healthCheckGracePeriodSeconds: typing.Optional[int]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    loadBalancers: typing.Optional[typing.List[LoadBalancer]]
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    pendingCount: typing.Optional[int]
    placementConstraints: typing.Optional[typing.List[PlacementConstraint]]
    placementStrategy: typing.Optional[typing.List[PlacementStrategy]]
    platformVersion: typing.Optional[str]
    propagateTags: typing.Optional[typing.Literal['TASK_DEFINITION', 'SERVICE']] = field()
    roleArn: typing.Optional[str]
    runningCount: typing.Optional[int]
    schedulingStrategy: typing.Optional[typing.Literal['REPLICA', 'DAEMON']] = field()
    serviceArn: typing.Optional[str]
    serviceName: typing.Optional[str]
    serviceRegistries: typing.Optional[typing.List[ServiceRegistry]]
    status: typing.Optional[str]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: typing.Optional[str]
    taskSets: typing.Optional[typing.List[TaskSet]]


class TaskSet:
    """
    Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an <code>EXTERNAL</code> deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
    """

    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    clusterArn: typing.Optional[str]
    computedDesiredCount: typing.Optional[int]
    createdAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    externalId: typing.Optional[str]
    id: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    loadBalancers: typing.Optional[typing.List[LoadBalancer]]
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    pendingCount: typing.Optional[int]
    platformVersion: typing.Optional[str]
    runningCount: typing.Optional[int]
    scale: typing.Optional[Scale] = field(metadata={'openapi': {'description': 'A floating-point percentage of the desired number of tasks to place and keep running in the task set.'}})
    serviceArn: typing.Optional[str]
    serviceRegistries: typing.Optional[typing.List[ServiceRegistry]]
    stabilityStatus: typing.Optional[typing.Literal['STEADY_STATE', 'STABILIZING']] = field()
    stabilityStatusAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    startedBy: typing.Optional[str]
    status: typing.Optional[str]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: typing.Optional[str]
    taskSetArn: typing.Optional[str]
    updatedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})


class Scale:
    """
    A floating-point percentage of the desired number of tasks to place and keep running in the task set.
    """

    unit: typing.Optional[typing.Literal['PERCENT']] = field()
    value: typing.Optional[float] = field(metadata={'openapi': {'format': 'double'}})


class ServiceEvent:
    """
    Details on an event associated with a service.
    """

    createdAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    id: typing.Optional[str]
    message: typing.Optional[str]


class Deployment:
    """
    The details of an Amazon ECS service deployment. This is used only when a service uses the <code>ECS</code> deployment controller type.
    """

    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    createdAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    desiredCount: typing.Optional[int]
    failedTasks: typing.Optional[int]
    id: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    pendingCount: typing.Optional[int]
    platformVersion: typing.Optional[str]
    rolloutState: typing.Optional[typing.Literal['COMPLETED', 'FAILED', 'IN_PROGRESS']] = field()
    rolloutStateReason: typing.Optional[str]
    runningCount: typing.Optional[int]
    status: typing.Optional[str]
    taskDefinition: typing.Optional[str]
    updatedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})


class CreateTaskSetRequest:
    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    clientToken: typing.Optional[str]
    cluster: str
    externalId: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    loadBalancers: typing.Optional[typing.List[LoadBalancer]]
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    platformVersion: typing.Optional[str]
    scale: typing.Optional[Scale] = field(metadata={'openapi': {'description': 'A floating-point percentage of the desired number of tasks to place and keep running in the task set.'}})
    service: str
    serviceRegistries: typing.Optional[typing.List[ServiceRegistry]]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: str


class CreateTaskSetResponse:
    taskSet: typing.Optional[TaskSet] = field(metadata={'openapi': {'description': 'Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an <code>EXTERNAL</code> deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.'}})


class DeleteAccountSettingRequest:
    name: typing.Literal['serviceLongArnFormat', 'taskLongArnFormat', 'containerInstanceLongArnFormat', 'awsvpcTrunking', 'containerInsights'] = field()
    principalArn: typing.Optional[str]


class DeleteAccountSettingResponse:
    setting: typing.Optional[Setting] = field(metadata={'openapi': {'description': 'The current account setting for a resource.'}})


class Setting:
    """
    The current account setting for a resource.
    """

    name: typing.Optional[typing.Literal['serviceLongArnFormat', 'taskLongArnFormat', 'containerInstanceLongArnFormat', 'awsvpcTrunking', 'containerInsights']] = field()
    principalArn: typing.Optional[str]
    value: typing.Optional[str]


class DeleteAttributesRequest:
    attributes: typing.List[Attribute]
    cluster: typing.Optional[str]


class DeleteAttributesResponse:
    attributes: typing.Optional[typing.List[Attribute]]


class DeleteCapacityProviderRequest:
    capacityProvider: str


class DeleteCapacityProviderResponse:
    capacityProvider: typing.Optional[CapacityProvider] = field(metadata={'openapi': {'description': 'The details of a capacity provider.'}})


class DeleteClusterRequest:
    cluster: str


class DeleteClusterResponse:
    cluster: typing.Optional[Cluster] = field(metadata={'openapi': {'description': 'A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.'}})


class DeleteServiceRequest:
    cluster: typing.Optional[str]
    force: typing.Optional[bool]
    service: str


class DeleteServiceResponse:
    service: typing.Optional[Service] = field(metadata={'openapi': {'description': 'Details on a service within a cluster'}})


class DeleteTaskSetRequest:
    cluster: str
    force: typing.Optional[bool]
    service: str
    taskSet: str


class DeleteTaskSetResponse:
    taskSet: typing.Optional[TaskSet] = field(metadata={'openapi': {'description': 'Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an <code>EXTERNAL</code> deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.'}})


class DeregisterContainerInstanceRequest:
    cluster: typing.Optional[str]
    containerInstance: str
    force: typing.Optional[bool]


class DeregisterContainerInstanceResponse:
    containerInstance: typing.Optional[ContainerInstance] = field(metadata={'openapi': {'description': 'An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.'}})


class DeregisterTaskDefinitionRequest:
    taskDefinition: str


class DeregisterTaskDefinitionResponse:
    taskDefinition: typing.Optional[TaskDefinition] = field(metadata={'openapi': {'description': 'The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.'}})


class TaskDefinition:
    """
    The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.
    """

    compatibilities: typing.Optional[typing.List[typing.Literal['EC2', 'FARGATE']]]
    containerDefinitions: typing.Optional[typing.List[ContainerDefinition]]
    cpu: typing.Optional[str]
    deregisteredAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    executionRoleArn: typing.Optional[str]
    family: typing.Optional[str]
    inferenceAccelerators: typing.Optional[typing.List[InferenceAccelerator]]
    ipcMode: typing.Optional[typing.Literal['host', 'task', 'none']] = field()
    memory: typing.Optional[str]
    networkMode: typing.Optional[typing.Literal['bridge', 'host', 'awsvpc', 'none']] = field()
    pidMode: typing.Optional[typing.Literal['host', 'task']] = field()
    placementConstraints: typing.Optional[typing.List[TaskDefinitionPlacementConstraint]]
    proxyConfiguration: typing.Optional[ProxyConfiguration] = field(metadata={'openapi': {'description': '<p>The configuration details for the App Mesh proxy.</p> <p>For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the <code>ecs-init</code> package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version <code>20190301</code> or later, then they contain the required versions of the container agent and <code>ecs-init</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html">Amazon ECS-optimized Linux AMI</a> </p>'}})
    registeredAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    registeredBy: typing.Optional[str]
    requiresAttributes: typing.Optional[typing.List[Attribute]]
    requiresCompatibilities: typing.Optional[typing.List[typing.Literal['EC2', 'FARGATE']]]
    revision: typing.Optional[int]
    status: typing.Optional[typing.Literal['ACTIVE', 'INACTIVE']] = field()
    taskDefinitionArn: typing.Optional[str]
    taskRoleArn: typing.Optional[str]
    volumes: typing.Optional[typing.List[Volume]]


class Volume:
    """
    A data volume used in a task definition. For tasks that use the Amazon Elastic File System (Amazon EFS), specify an <code>efsVolumeConfiguration</code>. For Windows tasks that use Amazon FSx for Windows File Server file system, specify a <code>fsxWindowsFileServerVolumeConfiguration</code>. For tasks that use a Docker volume, specify a <code>DockerVolumeConfiguration</code>. For tasks that use a bind mount host volume, specify a <code>host</code> and optional <code>sourcePath</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_data_volumes.html">Using Data Volumes in Tasks</a>.
    """

    dockerVolumeConfiguration: typing.Optional[DockerVolumeConfiguration] = field(metadata={'openapi': {'description': 'This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the <code>local</code> driver. To use bind mounts, specify a <code>host</code> instead.'}})
    efsVolumeConfiguration: typing.Optional[EFSVolumeConfiguration] = field(metadata={'openapi': {'description': 'This parameter is specified when you are using an Amazon Elastic File System file system for task storage. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html">Amazon EFS Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.'}})
    fsxWindowsFileServerVolumeConfiguration: typing.Optional[FSxWindowsFileServerVolumeConfiguration] = field(metadata={'openapi': {'description': '<p>This parameter is specified when you are using <a href="https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html">Amazon FSx for Windows File Server</a> file system for task storage.</p> <p>For more information and the input format, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html">Amazon FSx for Windows File Server Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>'}})
    host: typing.Optional[HostVolumeProperties] = field(metadata={'openapi': {'description': 'Details on a container instance bind mount host volume.'}})
    name: typing.Optional[str]


class HostVolumeProperties:
    """
    Details on a container instance bind mount host volume.
    """

    sourcePath: typing.Optional[str]


class FSxWindowsFileServerVolumeConfiguration:
    """
    <p>This parameter is specified when you are using <a href="https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html">Amazon FSx for Windows File Server</a> file system for task storage.</p> <p>For more information and the input format, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html">Amazon FSx for Windows File Server Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
    """

    authorizationConfig: FSxWindowsFileServerAuthorizationConfig = field(metadata={'openapi': {'description': '<p>The authorization configuration details for Amazon FSx for Windows File Server file system. See <a href="https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html">FSxWindowsFileServerVolumeConfiguration</a> in the <i>Amazon Elastic Container Service API Reference</i>.</p> <p>For more information and the input format, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html">Amazon FSx for Windows File Server Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>'}})
    fileSystemId: str
    rootDirectory: str


class FSxWindowsFileServerAuthorizationConfig:
    """
    <p>The authorization configuration details for Amazon FSx for Windows File Server file system. See <a href="https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html">FSxWindowsFileServerVolumeConfiguration</a> in the <i>Amazon Elastic Container Service API Reference</i>.</p> <p>For more information and the input format, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html">Amazon FSx for Windows File Server Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
    """

    credentialsParameter: str
    domain: str


class EFSVolumeConfiguration:
    """
    This parameter is specified when you are using an Amazon Elastic File System file system for task storage. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html">Amazon EFS Volumes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    authorizationConfig: typing.Optional[EFSAuthorizationConfig] = field(metadata={'openapi': {'description': 'The authorization configuration details for the Amazon EFS file system.'}})
    fileSystemId: str
    rootDirectory: typing.Optional[str]
    transitEncryption: typing.Optional[typing.Literal['ENABLED', 'DISABLED']] = field()
    transitEncryptionPort: typing.Optional[int]


class EFSAuthorizationConfig:
    """
    The authorization configuration details for the Amazon EFS file system.
    """

    accessPointId: typing.Optional[str]
    iam: typing.Optional[typing.Literal['ENABLED', 'DISABLED']] = field()


class DockerVolumeConfiguration:
    """
    This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the <code>local</code> driver. To use bind mounts, specify a <code>host</code> instead.
    """

    autoprovision: typing.Optional[bool]
    driver: typing.Optional[str]
    driverOpts: typing.Optional[typing.Dict[str, str]]
    labels: typing.Optional[typing.Dict[str, str]]
    scope: typing.Optional[typing.Literal['task', 'shared']] = field()


class ProxyConfiguration:
    """
    <p>The configuration details for the App Mesh proxy.</p> <p>For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the <code>ecs-init</code> package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version <code>20190301</code> or later, then they contain the required versions of the container agent and <code>ecs-init</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html">Amazon ECS-optimized Linux AMI</a> </p>
    """

    containerName: str
    properties: typing.Optional[typing.List[KeyValuePair]]
    type: typing.Optional[typing.Literal['APPMESH']] = field()


class TaskDefinitionPlacementConstraint:
    """
    <p>An object representing a constraint on task placement in the task definition. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <note> <p>If you are using the Fargate launch type, task placement constraints are not supported.</p> </note>
    """

    expression: typing.Optional[str]
    type: typing.Optional[typing.Literal['memberOf']] = field()


class InferenceAccelerator:
    """
    Details on a Elastic Inference accelerator. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html">Working with Amazon Elastic Inference on Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    deviceName: str
    deviceType: str


class DescribeCapacityProvidersRequest:
    capacityProviders: typing.Optional[typing.List[str]]
    include: typing.Optional[typing.List[typing.Literal['TAGS']]]
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]


class DescribeCapacityProvidersResponse:
    capacityProviders: typing.Optional[typing.List[CapacityProvider]]
    failures: typing.Optional[typing.List[Failure]]
    nextToken: typing.Optional[str]


class Failure:
    """
    A failed resource. For a list of common causes, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html">API failure reasons</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    arn: typing.Optional[str]
    detail: typing.Optional[str]
    reason: typing.Optional[str]


class DescribeClustersRequest:
    clusters: typing.Optional[typing.List[str]]
    include: typing.Optional[typing.List[typing.Literal['ATTACHMENTS', 'SETTINGS', 'STATISTICS', 'TAGS']]]


class DescribeClustersResponse:
    clusters: typing.Optional[typing.List[Cluster]]
    failures: typing.Optional[typing.List[Failure]]


class DescribeContainerInstancesRequest:
    cluster: typing.Optional[str]
    containerInstances: typing.List[str]
    include: typing.Optional[typing.List[typing.Literal['TAGS']]]


class DescribeContainerInstancesResponse:
    containerInstances: typing.Optional[typing.List[ContainerInstance]]
    failures: typing.Optional[typing.List[Failure]]


class DescribeServicesRequest:
    cluster: typing.Optional[str]
    include: typing.Optional[typing.List[typing.Literal['TAGS']]]
    services: typing.List[str]


class DescribeServicesResponse:
    failures: typing.Optional[typing.List[Failure]]
    services: typing.Optional[typing.List[Service]]


class DescribeTaskDefinitionRequest:
    include: typing.Optional[typing.List[typing.Literal['TAGS']]]
    taskDefinition: str


class DescribeTaskDefinitionResponse:
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: typing.Optional[TaskDefinition] = field(metadata={'openapi': {'description': 'The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.'}})


class DescribeTaskSetsRequest:
    cluster: str
    include: typing.Optional[typing.List[typing.Literal['TAGS']]]
    service: str
    taskSets: typing.Optional[typing.List[str]]


class DescribeTaskSetsResponse:
    failures: typing.Optional[typing.List[Failure]]
    taskSets: typing.Optional[typing.List[TaskSet]]


class DescribeTasksRequest:
    cluster: typing.Optional[str]
    include: typing.Optional[typing.List[typing.Literal['TAGS']]]
    tasks: typing.List[str]


class DescribeTasksResponse:
    failures: typing.Optional[typing.List[Failure]]
    tasks: typing.Optional[typing.List[Task]]


class Task:
    """
    Details on a task in a cluster.
    """

    attachments: typing.Optional[typing.List[Attachment]]
    attributes: typing.Optional[typing.List[Attribute]]
    availabilityZone: typing.Optional[str]
    capacityProviderName: typing.Optional[str]
    clusterArn: typing.Optional[str]
    connectivity: typing.Optional[typing.Literal['CONNECTED', 'DISCONNECTED']] = field()
    connectivityAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    containerInstanceArn: typing.Optional[str]
    containers: typing.Optional[typing.List[Container]]
    cpu: typing.Optional[str]
    createdAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    desiredStatus: typing.Optional[str]
    executionStoppedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    group: typing.Optional[str]
    healthStatus: typing.Optional[typing.Literal['HEALTHY', 'UNHEALTHY', 'UNKNOWN']] = field()
    inferenceAccelerators: typing.Optional[typing.List[InferenceAccelerator]]
    lastStatus: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    memory: typing.Optional[str]
    overrides: typing.Optional[TaskOverride] = field(metadata={'openapi': {'description': 'The overrides associated with a task.'}})
    platformVersion: typing.Optional[str]
    pullStartedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    pullStoppedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    startedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    startedBy: typing.Optional[str]
    stopCode: typing.Optional[typing.Literal['TaskFailedToStart', 'EssentialContainerExited', 'UserInitiated']] = field()
    stoppedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    stoppedReason: typing.Optional[str]
    stoppingAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskArn: typing.Optional[str]
    taskDefinitionArn: typing.Optional[str]
    version: typing.Optional[int]


class TaskOverride:
    """
    The overrides associated with a task.
    """

    containerOverrides: typing.Optional[typing.List[ContainerOverride]]
    cpu: typing.Optional[str]
    executionRoleArn: typing.Optional[str]
    inferenceAcceleratorOverrides: typing.Optional[typing.List[InferenceAcceleratorOverride]]
    memory: typing.Optional[str]
    taskRoleArn: typing.Optional[str]


class InferenceAcceleratorOverride:
    """
    Details on an Elastic Inference accelerator task override. This parameter is used to override the Elastic Inference accelerator specified in the task definition. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html">Working with Amazon Elastic Inference on Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.
    """

    deviceName: typing.Optional[str]
    deviceType: typing.Optional[str]


class DiscoverPollEndpointRequest:
    cluster: typing.Optional[str]
    containerInstance: typing.Optional[str]


class DiscoverPollEndpointResponse:
    endpoint: typing.Optional[str]
    telemetryEndpoint: typing.Optional[str]


class ListAccountSettingsRequest:
    effectiveSettings: typing.Optional[bool]
    maxResults: typing.Optional[int]
    name: typing.Optional[typing.Literal['serviceLongArnFormat', 'taskLongArnFormat', 'containerInstanceLongArnFormat', 'awsvpcTrunking', 'containerInsights']] = field()
    nextToken: typing.Optional[str]
    principalArn: typing.Optional[str]
    value: typing.Optional[str]


class ListAccountSettingsResponse:
    nextToken: typing.Optional[str]
    settings: typing.Optional[typing.List[Setting]]


class ListAttributesRequest:
    attributeName: typing.Optional[str]
    attributeValue: typing.Optional[str]
    cluster: typing.Optional[str]
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]
    targetType: typing.Literal['container-instance'] = field()


class ListAttributesResponse:
    attributes: typing.Optional[typing.List[Attribute]]
    nextToken: typing.Optional[str]


class ListClustersRequest:
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]


class ListClustersResponse:
    clusterArns: typing.Optional[typing.List[str]]
    nextToken: typing.Optional[str]


class ListContainerInstancesRequest:
    cluster: typing.Optional[str]
    filter: typing.Optional[str]
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]
    status: typing.Optional[typing.Literal['ACTIVE', 'DRAINING', 'REGISTERING', 'DEREGISTERING', 'REGISTRATION_FAILED']] = field()


class ListContainerInstancesResponse:
    containerInstanceArns: typing.Optional[typing.List[str]]
    nextToken: typing.Optional[str]


class ListServicesRequest:
    cluster: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]
    schedulingStrategy: typing.Optional[typing.Literal['REPLICA', 'DAEMON']] = field()


class ListServicesResponse:
    nextToken: typing.Optional[str]
    serviceArns: typing.Optional[typing.List[str]]


class ListTagsForResourceRequest:
    resourceArn: str


class ListTagsForResourceResponse:
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})


class ListTaskDefinitionFamiliesRequest:
    familyPrefix: typing.Optional[str]
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]
    status: typing.Optional[typing.Literal['ACTIVE', 'INACTIVE', 'ALL']] = field()


class ListTaskDefinitionFamiliesResponse:
    families: typing.Optional[typing.List[str]]
    nextToken: typing.Optional[str]


class ListTaskDefinitionsRequest:
    familyPrefix: typing.Optional[str]
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]
    sort: typing.Optional[typing.Literal['ASC', 'DESC']] = field()
    status: typing.Optional[typing.Literal['ACTIVE', 'INACTIVE']] = field()


class ListTaskDefinitionsResponse:
    nextToken: typing.Optional[str]
    taskDefinitionArns: typing.Optional[typing.List[str]]


class ListTasksRequest:
    cluster: typing.Optional[str]
    containerInstance: typing.Optional[str]
    desiredStatus: typing.Optional[typing.Literal['RUNNING', 'PENDING', 'STOPPED']] = field()
    family: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    maxResults: typing.Optional[int]
    nextToken: typing.Optional[str]
    serviceName: typing.Optional[str]
    startedBy: typing.Optional[str]


class ListTasksResponse:
    nextToken: typing.Optional[str]
    taskArns: typing.Optional[typing.List[str]]


class PlatformDevice:
    """
    The devices that are available on the container instance. The only supported device type is a GPU.
    """

    id: str
    type: typing.Literal['GPU'] = field()


class PutAccountSettingDefaultRequest:
    name: typing.Literal['serviceLongArnFormat', 'taskLongArnFormat', 'containerInstanceLongArnFormat', 'awsvpcTrunking', 'containerInsights'] = field()
    value: str


class PutAccountSettingDefaultResponse:
    setting: typing.Optional[Setting] = field(metadata={'openapi': {'description': 'The current account setting for a resource.'}})


class PutAccountSettingRequest:
    name: typing.Literal['serviceLongArnFormat', 'taskLongArnFormat', 'containerInstanceLongArnFormat', 'awsvpcTrunking', 'containerInsights'] = field()
    principalArn: typing.Optional[str]
    value: str


class PutAccountSettingResponse:
    setting: typing.Optional[Setting] = field(metadata={'openapi': {'description': 'The current account setting for a resource.'}})


class PutAttributesRequest:
    attributes: typing.List[Attribute]
    cluster: typing.Optional[str]


class PutAttributesResponse:
    attributes: typing.Optional[typing.List[Attribute]]


class PutClusterCapacityProvidersRequest:
    capacityProviders: typing.List[str]
    cluster: str
    defaultCapacityProviderStrategy: typing.List[CapacityProviderStrategyItem]


class PutClusterCapacityProvidersResponse:
    cluster: typing.Optional[Cluster] = field(metadata={'openapi': {'description': 'A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.'}})


class RegisterContainerInstanceRequest:
    attributes: typing.Optional[typing.List[Attribute]]
    cluster: typing.Optional[str]
    containerInstanceArn: typing.Optional[str]
    instanceIdentityDocument: typing.Optional[str]
    instanceIdentityDocumentSignature: typing.Optional[str]
    platformDevices: typing.Optional[typing.List[PlatformDevice]]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    totalResources: typing.Optional[typing.List[Resource]]
    versionInfo: typing.Optional[VersionInfo] = field(metadata={'openapi': {'description': 'The Docker and Amazon ECS container agent version information about a container instance.'}})


class RegisterContainerInstanceResponse:
    containerInstance: typing.Optional[ContainerInstance] = field(metadata={'openapi': {'description': 'An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.'}})


class RegisterTaskDefinitionRequest:
    containerDefinitions: typing.List[ContainerDefinition]
    cpu: typing.Optional[str]
    executionRoleArn: typing.Optional[str]
    family: str
    inferenceAccelerators: typing.Optional[typing.List[InferenceAccelerator]]
    ipcMode: typing.Optional[typing.Literal['host', 'task', 'none']] = field()
    memory: typing.Optional[str]
    networkMode: typing.Optional[typing.Literal['bridge', 'host', 'awsvpc', 'none']] = field()
    pidMode: typing.Optional[typing.Literal['host', 'task']] = field()
    placementConstraints: typing.Optional[typing.List[TaskDefinitionPlacementConstraint]]
    proxyConfiguration: typing.Optional[ProxyConfiguration] = field(metadata={'openapi': {'description': '<p>The configuration details for the App Mesh proxy.</p> <p>For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the <code>ecs-init</code> package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version <code>20190301</code> or later, then they contain the required versions of the container agent and <code>ecs-init</code>. For more information, see <a href="https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html">Amazon ECS-optimized Linux AMI</a> </p>'}})
    requiresCompatibilities: typing.Optional[typing.List[typing.Literal['EC2', 'FARGATE']]]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskRoleArn: typing.Optional[str]
    volumes: typing.Optional[typing.List[Volume]]


class RegisterTaskDefinitionResponse:
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: typing.Optional[TaskDefinition] = field(metadata={'openapi': {'description': 'The details of a task definition which describes the container and volume definitions of an Amazon Elastic Container Service task. You can specify which Docker images to use, the required resources, and other configurations related to launching the task definition through an Amazon ECS service or task.'}})


class RunTaskRequest:
    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    cluster: typing.Optional[str]
    count: typing.Optional[int]
    enableECSManagedTags: typing.Optional[bool]
    group: typing.Optional[str]
    launchType: typing.Optional[typing.Literal['EC2', 'FARGATE']] = field()
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    overrides: typing.Optional[TaskOverride] = field(metadata={'openapi': {'description': 'The overrides associated with a task.'}})
    placementConstraints: typing.Optional[typing.List[PlacementConstraint]]
    placementStrategy: typing.Optional[typing.List[PlacementStrategy]]
    platformVersion: typing.Optional[str]
    propagateTags: typing.Optional[typing.Literal['TASK_DEFINITION', 'SERVICE']] = field()
    referenceId: typing.Optional[str]
    startedBy: typing.Optional[str]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: str


class RunTaskResponse:
    failures: typing.Optional[typing.List[Failure]]
    tasks: typing.Optional[typing.List[Task]]


class StartTaskRequest:
    cluster: typing.Optional[str]
    containerInstances: typing.List[str]
    enableECSManagedTags: typing.Optional[bool]
    group: typing.Optional[str]
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    overrides: typing.Optional[TaskOverride] = field(metadata={'openapi': {'description': 'The overrides associated with a task.'}})
    propagateTags: typing.Optional[typing.Literal['TASK_DEFINITION', 'SERVICE']] = field()
    referenceId: typing.Optional[str]
    startedBy: typing.Optional[str]
    tags: typing.Optional[typing.List[Tag]] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})
    taskDefinition: str


class StartTaskResponse:
    failures: typing.Optional[typing.List[Failure]]
    tasks: typing.Optional[typing.List[Task]]


class StopTaskRequest:
    cluster: typing.Optional[str]
    reason: typing.Optional[str]
    task: str


class StopTaskResponse:
    task: typing.Optional[Task] = field(metadata={'openapi': {'description': 'Details on a task in a cluster.'}})


class SubmitAttachmentStateChangesRequest:
    attachments: typing.List[AttachmentStateChange]
    cluster: typing.Optional[str]


class SubmitAttachmentStateChangesResponse:
    acknowledgment: typing.Optional[str]


class SubmitContainerStateChangeRequest:
    cluster: typing.Optional[str]
    containerName: typing.Optional[str]
    exitCode: typing.Optional[int]
    networkBindings: typing.Optional[typing.List[NetworkBinding]]
    reason: typing.Optional[str]
    runtimeId: typing.Optional[str]
    status: typing.Optional[str]
    task: typing.Optional[str]


class SubmitContainerStateChangeResponse:
    acknowledgment: typing.Optional[str]


class SubmitTaskStateChangeRequest:
    attachments: typing.Optional[typing.List[AttachmentStateChange]]
    cluster: typing.Optional[str]
    containers: typing.Optional[typing.List[ContainerStateChange]]
    executionStoppedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    pullStartedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    pullStoppedAt: typing.Optional[datetime.datetime] = field(metadata={'openapi': {'format': 'date-time'}})
    reason: typing.Optional[str]
    status: typing.Optional[str]
    task: typing.Optional[str]


class SubmitTaskStateChangeResponse:
    acknowledgment: typing.Optional[str]


class TagResourceRequest:
    resourceArn: str
    tags: typing.List[Tag] = field(metadata={'openapi': {'maxItems': 50, 'minItems': 0}})


class TagResourceResponse:
    pass


class UntagResourceRequest:
    resourceArn: str
    tagKeys: typing.List[str]


class UntagResourceResponse:
    pass


class UpdateCapacityProviderRequest:
    autoScalingGroupProvider: AutoScalingGroupProviderUpdate = field(metadata={'openapi': {'description': 'The details of the Auto Scaling group capacity provider to update.'}})
    name: str


class UpdateCapacityProviderResponse:
    capacityProvider: typing.Optional[CapacityProvider] = field(metadata={'openapi': {'description': 'The details of a capacity provider.'}})


class UpdateClusterSettingsRequest:
    cluster: str
    settings: typing.List[ClusterSetting]


class UpdateClusterSettingsResponse:
    cluster: typing.Optional[Cluster] = field(metadata={'openapi': {'description': 'A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.'}})


class UpdateContainerAgentRequest:
    cluster: typing.Optional[str]
    containerInstance: str


class UpdateContainerAgentResponse:
    containerInstance: typing.Optional[ContainerInstance] = field(metadata={'openapi': {'description': 'An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.'}})


class UpdateContainerInstancesStateRequest:
    cluster: typing.Optional[str]
    containerInstances: typing.List[str]
    status: typing.Literal['ACTIVE', 'DRAINING', 'REGISTERING', 'DEREGISTERING', 'REGISTRATION_FAILED'] = field()


class UpdateContainerInstancesStateResponse:
    containerInstances: typing.Optional[typing.List[ContainerInstance]]
    failures: typing.Optional[typing.List[Failure]]


class UpdateServicePrimaryTaskSetRequest:
    cluster: str
    primaryTaskSet: str
    service: str


class UpdateServicePrimaryTaskSetResponse:
    taskSet: typing.Optional[TaskSet] = field(metadata={'openapi': {'description': 'Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an <code>EXTERNAL</code> deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.'}})


class UpdateServiceRequest:
    capacityProviderStrategy: typing.Optional[typing.List[CapacityProviderStrategyItem]]
    cluster: typing.Optional[str]
    deploymentConfiguration: typing.Optional[DeploymentConfiguration] = field(metadata={'openapi': {'description': 'Optional deployment parameters that control how many tasks run during a deployment and the ordering of stopping and starting tasks.'}})
    desiredCount: typing.Optional[int]
    forceNewDeployment: typing.Optional[bool]
    healthCheckGracePeriodSeconds: typing.Optional[int]
    networkConfiguration: typing.Optional[NetworkConfiguration] = field(metadata={'openapi': {'description': 'An object representing the network configuration for a task or service.'}})
    placementConstraints: typing.Optional[typing.List[PlacementConstraint]]
    placementStrategy: typing.Optional[typing.List[PlacementStrategy]]
    platformVersion: typing.Optional[str]
    service: str
    taskDefinition: typing.Optional[str]


class UpdateServiceResponse:
    service: typing.Optional[Service] = field(metadata={'openapi': {'description': 'Details on a service within a cluster'}})


class UpdateTaskSetRequest:
    cluster: str
    scale: Scale = field(metadata={'openapi': {'description': 'A floating-point percentage of the desired number of tasks to place and keep running in the task set.'}})
    service: str
    taskSet: str


class UpdateTaskSetResponse:
    taskSet: typing.Optional[TaskSet] = field(metadata={'openapi': {'description': 'Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an <code>EXTERNAL</code> deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.'}})
